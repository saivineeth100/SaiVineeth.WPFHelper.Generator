using SaiVineeth.WPFHelper.Generator.Extensions.Symbols;

namespace SaiVineeth.WPFHelper.Generator.Generators;
[Generator(LanguageNames.CSharp)]
public class MVVMHelpersGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        //if (!Debugger.IsAttached)
        //{
        //    Debugger.Launch();
        //}

        IncrementalValuesProvider<MVVMHelpersGeneratorArgs> syntaxProvider = context.SyntaxProvider
            .CreateSyntaxProvider(SyntaxPredicate, SematicTransform)
            .Where(static (type) => type != null)!;
        IncrementalValueProvider<ProjectArgs> projectDirProvider = context.AnalyzerConfigOptionsProvider
            .Select(static (provider, _) =>
            {
                provider.GlobalOptions.TryGetValue("build_property.projectdir", out string? projectDirectory);
                provider.GlobalOptions.TryGetValue("build_property.RootNamespace", out string? rootNamespace);
                return new ProjectArgs() { RootNamespace = rootNamespace!, ProjectRoot = projectDirectory! };
            });
        IncrementalValuesProvider<(MVVMHelpersGeneratorArgs Left, ProjectArgs Right)> incrementalValuesProvider = syntaxProvider.Combine(projectDirProvider);
        context.RegisterSourceOutput(incrementalValuesProvider, Execute);

    }



    private bool SyntaxPredicate(SyntaxNode node, CancellationToken token)
    {
        if (node is ClassDeclarationSyntax candidate)
        {
            if (candidate.Modifiers.Any(SyntaxKind.PartialKeyword) && candidate.Modifiers.Any(SyntaxKind.PublicKeyword))
            {
                if (candidate.AttributeLists.Count > 0)
                {
                    return true;
                }
            }
        }
        return false;
    }
    private MVVMHelpersGeneratorArgs SematicTransform(GeneratorSyntaxContext context, CancellationToken token)
    {
        
        ClassDeclarationSyntax classDeclaration = (ClassDeclarationSyntax)context.Node;
        INamedTypeSymbol? namedTypeSymbol = context.SemanticModel.GetDeclaredSymbol(classDeclaration);
        MVVMHelpersGeneratorArgs mVVMHelpersGeneratorArgs = new(namedTypeSymbol!.ContainingNamespace.ToString(), namedTypeSymbol.Name.ToString());
        List<MVVMHelperGeneratorArg> generatorArgs = new();
        mVVMHelpersGeneratorArgs.MVVMHelperGeneratorArgs = generatorArgs;
        if (namedTypeSymbol != null)
        {
            INamedTypeSymbol baseViewModelTypeSymbol = (INamedTypeSymbol)namedTypeSymbol
                .GetAttributes()
                .Where(c => c.AttributeClass!.GetClassMetaName() == "SaiVineeth.MVVMHelpers.Attributes.RegisterViews")
                .FirstOrDefault().AttributeClass!.TypeArguments.Last();
            mVVMHelpersGeneratorArgs.BaseViewModelTypeSymbol = baseViewModelTypeSymbol.GetClassMetaName();
            IEnumerable<AttributeSyntax> attributes = classDeclaration.AttributeLists.SelectMany(c => c.Attributes);
            foreach (var attribute in attributes)
            {
                if (attribute.Name is GenericNameSyntax genericNameSyntax)
                {
                    if (genericNameSyntax.Identifier.Text == "RegisterView")
                    {
                        SeparatedSyntaxList<TypeSyntax> arguments = genericNameSyntax.TypeArgumentList.Arguments;

                        if (arguments != null)
                        {
                            SeparatedSyntaxList<AttributeArgumentSyntax> attributeArguments = attribute.ArgumentList!.Arguments;
                            ExpressionSyntax expression = attributeArguments.First().Expression;

                            INamedTypeSymbol viewModel = (INamedTypeSymbol)context.SemanticModel.GetSymbolInfo(arguments[0]).Symbol!;
                            INamedTypeSymbol view = (INamedTypeSymbol)context.SemanticModel.GetSymbolInfo(arguments[1]).Symbol!;
                            generatorArgs.Add(new(viewModel, view, expression));
                        }
                    }
                }
            }
        }
        return mVVMHelpersGeneratorArgs;
    }
    private void Execute(SourceProductionContext context, (MVVMHelpersGeneratorArgs, ProjectArgs) data)
    {
        var args = data.Item1;
        var path = data.Item2.ProjectRoot;
        string mainNamespace = data.Item2.RootNamespace;
        CreateViewModelFactory(context, args);

        GenerateAppExtension(context, args, mainNamespace);
        GenerateViewModelExtension(context, args, mainNamespace);
    }

    private void GenerateAppExtension(SourceProductionContext context, MVVMHelpersGeneratorArgs args, string mainNamespace)
    {
        List<StatementSyntax> statements = [];

        List<SyntaxNodeOrToken> mainInitArgs = [];
        foreach (MVVMHelperGeneratorArg arg in args.MVVMHelperGeneratorArgs)
        {
            List<SyntaxNodeOrToken> collectionArgs = [];
            collectionArgs.Add(ObjectCreationExpression(GetGlobalNameforType("System.Windows.DataTemplateKey"))
                .WithArgumentList(ArgumentList(SingletonSeparatedList<ArgumentSyntax>(Argument(
                                                                                        TypeOfExpression(
                                                                                            IdentifierName(arg.ViewModel.OriginalDefinition.ToString())))))));
            collectionArgs.Add(Token(SyntaxKind.CommaToken));
            collectionArgs.Add(ObjectCreationExpression(GetGlobalNameforType("System.Windows.DataTemplate"))
                .WithArgumentList(ArgumentList())
                .WithInitializer(InitializerExpression(SyntaxKind.ObjectInitializerExpression, SingletonSeparatedList<ExpressionSyntax>(AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, IdentifierName("VisualTree"), ObjectCreationExpression(GetGlobalNameforType("System.Windows.FrameworkElementFactory"))
                .WithArgumentList(ArgumentList(SingletonSeparatedList<ArgumentSyntax>(Argument(TypeOfExpression(IdentifierName(arg.View.OriginalDefinition.ToString())))))))))));
            if (mainInitArgs.Count > 0)
            {
                mainInitArgs.Add(Token(SyntaxKind.CommaToken));
            }
            mainInitArgs.Add(InitializerExpression(SyntaxKind.ComplexElementInitializerExpression, SeparatedList<ExpressionSyntax>(collectionArgs)));
        }
        statements.Add(ExpressionStatement(InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, IdentifierName("app.Resources.MergedDictionaries"), IdentifierName("Add")))
            .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(ImplicitObjectCreationExpression()
            .WithArgumentList(ArgumentList().WithCloseParenToken(Token(TriviaList(), SyntaxKind.CloseParenToken, TriviaList(LineFeed))))
            .WithInitializer(InitializerExpression(SyntaxKind.CollectionInitializerExpression, SeparatedList<ExpressionSyntax>(mainInitArgs)))))))));
        var source = CompilationUnit()
            .WithMembers(SingletonList<MemberDeclarationSyntax>(
                FileScopedNamespaceDeclaration(IdentifierName(mainNamespace))
                .WithMembers(SingletonList<MemberDeclarationSyntax>(ClassDeclaration("AppExtension")
                .WithModifiers(TokenList(
                             [
                                 Token(SyntaxKind.PublicKeyword),
                                 Token(SyntaxKind.StaticKeyword),
                                 Token(SyntaxKind.PartialKeyword)
                             ]))
                //.WithBaseList(BaseList(SingletonSeparatedList<BaseTypeSyntax>(SimpleBaseType(GetGlobalNameforType("System.Windows.Application")))))
                .WithMembers(List(new MemberDeclarationSyntax[]
                {
                    MethodDeclaration(
                               PredefinedType(Token(SyntaxKind.VoidKeyword)),
                                Identifier("AddMVVMTemplatesResourceDict"))
                    .WithModifiers(TokenList(
                             [
                                 Token(SyntaxKind.PublicKeyword),
                                 Token(SyntaxKind.StaticKeyword),
                             ]))
                    .WithBody(Block(statements))
                    .WithParameterList(ParameterList(SeparatedList<ParameterSyntax>(new SyntaxNodeOrToken[]
                    {
                        Parameter(Identifier("app"))
                                .WithModifiers(
                                    TokenList(
                                        Token(SyntaxKind.ThisKeyword)))
                                .WithType(
                                    IdentifierName("System.Windows.Application"))
                    })))
                }))))))
            .NormalizeWhitespace()
            .ToFullString();
        context.AddSource("App.AddMVVMTemplatesResourceDict.g.cs", source);

    }

    private void GenerateViewModelExtension(SourceProductionContext context,
                                            MVVMHelpersGeneratorArgs args,
                                            string mainNamespace)
    {
        List<StatementSyntax> statementSyntaxes = new();
        foreach (var arg in args.MVVMHelperGeneratorArgs)
        {
            INamedTypeSymbol viewModel = arg.ViewModel;
            string fullViewmodelName = $"global::{viewModel.OriginalDefinition}";
            List<SyntaxNodeOrToken> syntaxNodeOrTokens = new();

            IMethodSymbol methodSymbol = arg.ViewModel.Constructors.Where(c => c.Parameters.Length > 0).FirstOrDefault();
            if (methodSymbol != null)
            {
                System.Collections.Immutable.ImmutableArray<IParameterSymbol> parameters = methodSymbol.Parameters;
                int length = parameters.Length;
                for (int i = 0; i < length; i++)
                {
                    var parameter = parameters[i];
                    string parameterTypeName = parameter.Type.ToString();
                    //if(parameter.Type is INamedTypeSymbol symbol && symbol.IsGenericType)
                    //{
                    //    parameterTypeName = parameter.Type.ToString();
                    //}
                    if (parameterTypeName == "System.Collections.Generic.Dictionary<string, object>?")
                    {
                        syntaxNodeOrTokens.Add(Argument(IdentifierName("args")));
                    }
                    else
                    {
                        syntaxNodeOrTokens.Add(Argument(
                            InvocationExpression(
                                 MemberAccessExpression(
                                     SyntaxKind.SimpleMemberAccessExpression,
                                     IdentifierName("global::Microsoft.Extensions.DependencyInjection.ServiceProviderServiceExtensions"),
                                     GenericName(
                                         Identifier("GetRequiredService"))
                                     .WithTypeArgumentList(
                                         TypeArgumentList(
                                             SingletonSeparatedList<TypeSyntax>(
                                                 IdentifierName($"global::{parameterTypeName}"))))))
                            .WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(IdentifierName("serviceProvider")))))));
                        if (i + 1 != length)
                        {
                            syntaxNodeOrTokens.Add(Token(SyntaxKind.CommaToken));
                        }
                    }
                }

            }

            InvocationExpressionSyntax invocationExpressionSyntax =
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        IdentifierName("global::Microsoft.Extensions.DependencyInjection.ServiceCollectionServiceExtensions"),
                        GenericName(Identifier("AddSingleton"))
                        .WithTypeArgumentList(
                            TypeArgumentList(SingletonSeparatedList(
                                (TypeSyntax)GenericName(Identifier("global::SaiVineeth.MVVMHelpers.Delegates.CreateViewModel"))
                                .WithTypeArgumentList(TypeArgumentList(
                                    SingletonSeparatedList((TypeSyntax)IdentifierName(fullViewmodelName)))))))))
                .WithArgumentList(ArgumentList(
                    SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]{
                            Argument(IdentifierName("services")),Token(SyntaxKind.CommaToken),
                        Argument(
                            SimpleLambdaExpression(Parameter(Identifier("serviceProvider")))
                            .WithBlock(Block(
                                SingletonList(
                                    (StatementSyntax)ReturnStatement(
                                        ParenthesizedLambdaExpression()
                                        .WithParameterList(
                                            ParameterList(
                                                SingletonSeparatedList(
                                                    Parameter(Identifier("args")))))
                                        .WithExpressionBody(
                                            ObjectCreationExpression(IdentifierName(fullViewmodelName))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SeparatedList<ArgumentSyntax>(syntaxNodeOrTokens)))))))))
                        })));
            statementSyntaxes.Add(ExpressionStatement(invocationExpressionSyntax));
        }

        statementSyntaxes.Add(ReturnStatement(IdentifierName("services")));
        CompilationUnitSyntax compilationUnitSyntax = CompilationUnit()
            .WithMembers(
            SingletonList<MemberDeclarationSyntax>(
                 FileScopedNamespaceDeclaration(
                     IdentifierName($"{mainNamespace}.Extensions"))
                 .WithMembers(
                     SingletonList<MemberDeclarationSyntax>(
                         ClassDeclaration("ServiceExtensions")
                         .WithModifiers(TokenList(
                             new[]{
                                 Token(SyntaxKind.PublicKeyword),
                                 Token(SyntaxKind.StaticKeyword),
                                 Token(SyntaxKind.PartialKeyword)
                             }))

                         .WithMembers(List(new MemberDeclarationSyntax[]
                         {
                              MethodDeclaration(
                                IdentifierName("global::Microsoft.Extensions.DependencyInjection.IServiceCollection"),
                                Identifier("AddViewModels"))
                              .WithModifiers(TokenList(new []
                              {
                                  Token(SyntaxKind.PublicKeyword),
                                  Token(SyntaxKind.StaticKeyword)
                              }))
                              .WithParameterList(ParameterList(
                                 SingletonSeparatedList(
                                     Parameter(Identifier("services"))
                                     .WithModifiers(
                                         TokenList(Token(SyntaxKind.ThisKeyword)))
                                     .WithType(IdentifierName("global::Microsoft.Extensions.DependencyInjection.IServiceCollection")))))
                              .WithBody(Block(
                                  statementSyntaxes
                                  ))
                         })))))
            ).NormalizeWhitespace();
        string source = compilationUnitSyntax.ToFullString();
        context.AddSource("ViewModelExtensions.g.cs", source);
    }



    private void CreateViewModelFactory(SourceProductionContext context, MVVMHelpersGeneratorArgs args)
    {
        List<MemberDeclarationSyntax> fields = new();
        List<SyntaxNodeOrToken> constructorParams = new();
        List<ExpressionStatementSyntax> assignmentStatements = new();
        List<SyntaxNodeOrToken> switchExpressions = new();
        List<MemberDeclarationSyntax> nodes = new();
        int maxCount = args.MVVMHelperGeneratorArgs.Count;
        int counter = 1;
        foreach (var arg in args.MVVMHelperGeneratorArgs)
        {

            INamedTypeSymbol viewModel = arg.ViewModel;
            string shortName = viewModel.Name;
            string fullName = $"global::{viewModel.OriginalDefinition}";
            GenericNameSyntax genericNameSyntax = GenericName(Identifier("global::SaiVineeth.MVVMHelpers.Delegates.CreateViewModel"))
                .WithTypeArgumentList(TypeArgumentList(SingletonSeparatedList<TypeSyntax>(IdentifierName(fullName))));


            string fieldName = $"_create{shortName}";
            fields.Add(FieldDeclaration(VariableDeclaration(genericNameSyntax)
                .WithVariables(SingletonSeparatedList(VariableDeclarator(Identifier(fieldName))))
               ).WithModifiers(TokenList(new[] { Token(SyntaxKind.PrivateKeyword), Token(SyntaxKind.ReadOnlyKeyword) })));

            constructorParams.Add(Parameter(Identifier($"create{shortName}")).WithType(genericNameSyntax));
            if (counter < maxCount)
            {
                constructorParams.Add(Token(SyntaxKind.CommaToken).WithTrailingTrivia(SyntaxTrivia(SyntaxKind.EndOfLineTrivia, string.Empty)));
            }
            assignmentStatements.Add(ExpressionStatement(
                                        AssignmentExpression(
                                            SyntaxKind.SimpleAssignmentExpression,
                                            IdentifierName(fieldName),
                                            IdentifierName($"create{shortName}"))));

            switchExpressions.Add(SwitchExpressionArm(
                            ConstantPattern(
                                arg.ViewTypeExpression),
                            InvocationExpression(
                                IdentifierName(fieldName))));
            switchExpressions.Add(Token(SyntaxKind.CommaToken));
            counter++;
        }
        nodes.AddRange(fields);

        ConstructorDeclarationSyntax constructorDeclaration = ConstructorDeclaration(Identifier(args.ClassName))
            .WithModifiers(TokenList(Token(SyntaxKind.PublicKeyword)))
            .WithParameterList(ParameterList(SeparatedList<ParameterSyntax>(constructorParams)))
            .WithBody(Block(assignmentStatements));

        nodes.Add(constructorDeclaration);
        switchExpressions.Add(SwitchExpressionArm(
            DiscardPattern(),
            ThrowExpression(
                 ObjectCreationExpression(
                     IdentifierName("ArgumentException"))
                 .WithArgumentList(
                     ArgumentList(
                         SeparatedList<ArgumentSyntax>(
                             new SyntaxNodeOrToken[]
                             {
                                 Argument(
                                     LiteralExpression(SyntaxKind.StringLiteralExpression,
                                                       Literal("The View type doesnot exists"))),
                                 Token(SyntaxKind.CommaToken),
                                 Argument(
                                     LiteralExpression(SyntaxKind.StringLiteralExpression,
                                                       Literal("View Type")))}))))));
        switchExpressions.Add(Token(SyntaxKind.CommaToken));
        MethodDeclarationSyntax createViewModelMethod = MethodDeclaration(
                          IdentifierName($"global::{args.BaseViewModelTypeSymbol}"),
                          Identifier("CreateViewModel"))
            .WithModifiers(TokenList(Token(SyntaxKind.PublicKeyword)))
            .WithParameterList(
            ParameterList(
                SeparatedList<ParameterSyntax>(
                    new SyntaxNodeOrToken[]
                    {
                        Parameter(
                            Identifier("viewType"))
                        .WithType(
                            IdentifierName("ViewType")),
                        Token(SyntaxKind.CommaToken),
                        Parameter(
                            Identifier("extraParams"))
                        .WithType(
                            NullableType(
                                GenericName(
                                    Identifier("Dictionary"))
                                .WithTypeArgumentList(
                                    TypeArgumentList(
                                        SeparatedList<TypeSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                PredefinedType(
                                                    Token(SyntaxKind.StringKeyword)),
                                                Token(SyntaxKind.CommaToken),
                                                PredefinedType(
                                                    Token(SyntaxKind.ObjectKeyword))})))))
                        .WithDefault(
                            EqualsValueClause(
                                LiteralExpression(
                                    SyntaxKind.NullLiteralExpression)))
                    })))
            .WithExpressionBody(
            ArrowExpressionClause(
                SwitchExpression(IdentifierName("viewType"))
                .WithArms(SeparatedList<SwitchExpressionArmSyntax>(switchExpressions)
                )))
            .WithSemicolonToken(Token(SyntaxKind.SemicolonToken));
        nodes.Add(createViewModelMethod);


        CompilationUnitSyntax compilationUnit = CompilationUnit()
            .WithMembers(
            SingletonList(
                (MemberDeclarationSyntax)FileScopedNamespaceDeclaration(IdentifierName(args.NameSpace))
                .WithNamespaceKeyword(Token(TriviaList(Trivia(
                    NullableDirectiveTrivia(Token(SyntaxKind.EnableKeyword), true))),
                SyntaxKind.NamespaceKeyword,
                TriviaList()))
                .WithMembers(SingletonList(
                    (MemberDeclarationSyntax)ClassDeclaration(args.ClassName)
                    .WithModifiers(TokenList(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.PartialKeyword)))
                    .WithMembers(List(nodes)))

                ))
            ).NormalizeWhitespace();
        string source = compilationUnit.ToFullString();
        context.AddSource("ViewModelFactory.g.cs", source);
    }

    internal AliasQualifiedNameSyntax GetGlobalNameforType(string typeName)
    {
        return AliasQualifiedName(IdentifierName(Token(SyntaxKind.GlobalKeyword)),
                                                                   IdentifierName(typeName));
    }
    private static VariableDeclarationSyntax CreateVariableDeclaration(string varName, ExpressionSyntax expressionSyntax)
    {
        IdentifierNameSyntax varSyntax = IdentifierName(Identifier(TriviaList(),
                                                                                  SyntaxKind.VarKeyword,
                                                                                  "var",
                                                                                  "var",
                                                                                  TriviaList()));
        return CreateVariableDelaration(varSyntax, varName, expressionSyntax);
    }

    private static VariableDeclarationSyntax CreateVariableDelaration(TypeSyntax varSyntax, string varName, ExpressionSyntax expressionSyntax)
    {
        return VariableDeclaration(varSyntax)
            .WithVariables(SingletonSeparatedList(VariableDeclarator(Identifier(varName))
            .WithInitializer(EqualsValueClause(expressionSyntax))
            ));
    }
}

public class MVVMHelpersGeneratorArgs
{
    public MVVMHelpersGeneratorArgs(string cnamespace, string className)
    {
        NameSpace = cnamespace;

        ClassName = className;
    }

    public string NameSpace { get; set; }
    public string ClassName { get; set; }

    public List<MVVMHelperGeneratorArg> MVVMHelperGeneratorArgs { get; set; }
    public string BaseViewModelTypeSymbol { get; set; }
}
public class MVVMHelperGeneratorArg
{
    public MVVMHelperGeneratorArg(INamedTypeSymbol viewModel, INamedTypeSymbol view, ExpressionSyntax expression)
    {
        ViewModel = viewModel;
        View = view;
        ViewTypeExpression = expression;
    }

    public INamedTypeSymbol ViewModel { get; set; }
    public INamedTypeSymbol View { get; set; }
    public ExpressionSyntax ViewTypeExpression { get; set; }
}
public class ProjectArgs
{
    public string ProjectRoot { get; set; }
    public string RootNamespace { get; set; }
}