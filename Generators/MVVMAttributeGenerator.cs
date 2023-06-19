using System.Collections.Immutable;
using SaiVineeth.WPFHelper.Generator.Extensions.Symbols;

namespace SaiVineeth.WPFHelper.Generator.Generators;
[Generator(LanguageNames.CSharp)]
public class MVVMAttributeGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        //if (!Debugger.IsAttached)
        //{
        //    Debugger.Launch();
        //}
        context.RegisterPostInitializationOutput(PostInitialization);
        IncrementalValuesProvider<MVVMAttributeGenerateArgs> syntaxProvider = context.SyntaxProvider
           .CreateSyntaxProvider(SyntaxPredicate, SematicTransform)
           .Where(static (type) => type != null)!;
        context.RegisterSourceOutput(syntaxProvider, Execute);

    }
    private void PostInitialization(IncrementalGeneratorPostInitializationContext context)
    {
        context.AddSource("SaiVineeth.DefaultAttribute.mvvm.g.cs", @"namespace SaiVineeth.MVVMHelpers.Attributes;

public class RegisterViews<ViewType,BaseViewType> : Attribute where ViewType : Enum where BaseViewType:class
{

}");
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
    private MVVMAttributeGenerateArgs? SematicTransform(GeneratorSyntaxContext context, CancellationToken token)
    {
        ClassDeclarationSyntax classDeclaration = (ClassDeclarationSyntax)context.Node;
        INamedTypeSymbol? namedTypeSymbol = context.SemanticModel.GetDeclaredSymbol(classDeclaration);
        if (namedTypeSymbol != null)
        {
            ImmutableArray<AttributeData> attributeDatas = namedTypeSymbol.GetAttributes();
            foreach (var attributeData in attributeDatas)
            {
                if (attributeData.GetAttrubuteMetaName() == "SaiVineeth.MVVMHelpers.Attributes.RegisterViews")
                {
                    ImmutableArray<ITypeSymbol> typeArguments = attributeData.AttributeClass!.TypeArguments;
                    if (typeArguments.Length == 2)
                    {
                        return new MVVMAttributeGenerateArgs(((INamedTypeSymbol)typeArguments[0]).GetClassMetaName(), ((INamedTypeSymbol)typeArguments[1]).GetClassMetaName());
                    }

                };
            }
        }
        return null;
    }

    private void Execute(SourceProductionContext context, MVVMAttributeGenerateArgs args)
    {
        context.AddSource("SaiVineeth.Attribute.mvvm.g.cs", @$"#nullable enable

namespace SaiVineeth.MVVMHelpers.Attributes;

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
public class RegisterView<ViewModelType, ViewControlType> : Attribute where ViewModelType : global::{args.BaseViewFullName} where ViewControlType : global::System.Windows.Controls.UserControl
{{    public RegisterView(global::{args.VieTypeFullName} viewType)
    {{
        ViewType = viewType;
    }}

    public global::{args.VieTypeFullName} ViewType {{ get; set; }}

}}");
        context.AddSource("SaiVineeth.Delegate.mvvm.g.cs", @$"#nullable enable

namespace SaiVineeth.MVVMHelpers.Delegates;

public delegate TViewModel CreateViewModel<TViewModel>(Dictionary<string, object>? extraParams = null) where TViewModel : global::{args.BaseViewFullName};");
    }

}
public class MVVMAttributeGenerateArgs
{
    public MVVMAttributeGenerateArgs(string viewType, string BaseView)
    {
        VieTypeFullName = viewType;
        BaseViewFullName = BaseView;
    }

    public string VieTypeFullName { get; set; }
    public string BaseViewFullName { get; set; }
}
