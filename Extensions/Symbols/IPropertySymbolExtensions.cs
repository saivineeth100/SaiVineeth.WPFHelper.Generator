﻿namespace SaiVineeth.WPFHelper.Generator.Extensions.Symbols;
public static class IPropertySymbolExtensions
{
    public static string? GetXmlTagFromPropertyAttributes(this IPropertySymbol propertySymbol)
    {
        System.Collections.Immutable.ImmutableArray<AttributeData> attributeData = propertySymbol.GetAttributes();
        foreach (AttributeData attributeDataAttribute in attributeData)
        {
            if (attributeDataAttribute.GetAttrubuteMetaName() == "System.Xml.Serialization.XmlElementAttribute")
            {
                if (attributeDataAttribute.ConstructorArguments != null && attributeDataAttribute.ConstructorArguments.Length > 0)
                {
                    var name = attributeDataAttribute.ConstructorArguments.FirstOrDefault().Value?.ToString();
                    if (name != null)
                    {
                        return name;
                    }
                }
            }
        }
        return null;
    }
    public static XMlProperties? GetXmlProperties(this IPropertySymbol propertySymbol)
    {
        System.Collections.Immutable.ImmutableArray<AttributeData> attributeData = propertySymbol.GetAttributes();
        foreach (AttributeData attributeDataAttribute in attributeData)
        {
            if (attributeDataAttribute.GetAttrubuteMetaName() == "System.Xml.Serialization.XmlElementAttribute")
            {
                if (attributeDataAttribute.ConstructorArguments != null && attributeDataAttribute.ConstructorArguments.Length > 0)
                {
                    var name = attributeDataAttribute.ConstructorArguments.FirstOrDefault().Value?.ToString();
                    if (name != null)
                    {
                        return new(name);
                    }
                }
            }

            if (attributeDataAttribute.GetAttrubuteMetaName() == "System.Xml.Serialization.XmlAttributeAttribute")
            {
                if (attributeDataAttribute.ConstructorArguments != null && attributeDataAttribute.ConstructorArguments.Length > 0)
                {
                    var name = attributeDataAttribute.ConstructorArguments.FirstOrDefault().Value?.ToString();
                    if (name != null)
                    {
                        return new(name, true);
                    }

                }
                if (attributeDataAttribute.NamedArguments != null && attributeDataAttribute.NamedArguments.Length > 0)
                {
                    System.Collections.Immutable.ImmutableArray<KeyValuePair<string, TypedConstant>> namedArguments = attributeDataAttribute.NamedArguments;
                    foreach (var namedArgument in namedArguments)
                    {
                        if (namedArgument.Key != null && namedArgument.Key == "AttributeName")
                        {
                            if (!namedArgument.Value.IsNull)
                            {
                                return new(namedArgument.Value.Value!.ToString(), true);
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    public static TDLFieldProperties? GetTDLFieldProperties(this IPropertySymbol propertySymbol)
    {
        System.Collections.Immutable.ImmutableArray<AttributeData> attributeData = propertySymbol.GetAttributes();
        foreach (AttributeData attributeDataAttribute in attributeData)
        {
            TDLFieldProperties tDLFieldProperties = new TDLFieldProperties();
            if (attributeDataAttribute.GetAttrubuteMetaName() == "TallyConnector.Core.Attributes.TDLXMLSetAttribute")
            {
                if (attributeDataAttribute.ConstructorArguments != null && attributeDataAttribute.ConstructorArguments.Length > 0)
                {
                    tDLFieldProperties.Set = (string?)attributeDataAttribute.ConstructorArguments.FirstOrDefault().Value;
                }
                if (attributeDataAttribute.NamedArguments != null && attributeDataAttribute.NamedArguments.Length > 0)
                {
                    System.Collections.Immutable.ImmutableArray<KeyValuePair<string, TypedConstant>> namedArguments = attributeDataAttribute.NamedArguments;
                    foreach (var namedArgument in namedArguments)
                    {
                        switch (namedArgument.Key)
                        {
                            case "Set":
                                tDLFieldProperties.Set = (string?)namedArgument.Value.Value;
                                break;
                            case "ExcludeInFetch":
                                tDLFieldProperties.ExcludeInFetch = (string?)namedArgument.Value.Value;
                                break;
                            case "Use":
                                tDLFieldProperties.Use = (string?)namedArgument.Value.Value;
                                break;
                            case "TallyType":
                                tDLFieldProperties.TallyType = (string?)namedArgument.Value.Value;
                                break;
                            case "Format":
                                tDLFieldProperties.Format = (string?)namedArgument.Value.Value;
                                break;
                        }

                    }
                }

                return tDLFieldProperties;
            }

        }
        return null;
    }
    public static void GetTDLCollectionProperties(this IPropertySymbol propertySymbol)
    {
        System.Collections.Immutable.ImmutableArray<AttributeData> attributeData = propertySymbol.GetAttributes();
        foreach (AttributeData attributeDataAttribute in attributeData)
        {
            if (attributeDataAttribute.GetAttrubuteMetaName() == "TallyConnector.Core.Attributes.TDLCollectionAttribute")
            {

            }
        }
    }
}
public class XMlProperties
{
    public XMlProperties(string xmlTag)
    {
        XMLTag = xmlTag;
    }
    public XMlProperties(string xmlTag, bool isAttribute)
    {
        XMLTag = xmlTag;
        IsAttribute = true;
    }

    public string XMLTag { get; set; }

    public bool IsAttribute { get; set; }
}


public class TDLFieldProperties
{
    public string? Set { get; set; }
    public string? ExcludeInFetch { get; set; }
    public string? Use { get; set; }
    public string? TallyType { get; set; }
    public string? Format { get; set; }
}