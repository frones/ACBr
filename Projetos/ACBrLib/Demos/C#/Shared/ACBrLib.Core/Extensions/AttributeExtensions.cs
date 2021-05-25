using System;
using System.Linq;
using System.Reflection;

namespace ACBrLib.Core.Extensions
{
    internal static class AttributeExtensions
    {
        public static TAttribute GetAttribute<TAttribute>(this ICustomAttributeProvider provider) where TAttribute : Attribute
        {
            var att = provider.GetCustomAttributes(typeof(TAttribute), true).FirstOrDefault() as TAttribute;
            return att;
        }

        public static bool HasAttribute<T>(this ICustomAttributeProvider provider) where T : Attribute
        {
            var atts = provider.GetCustomAttributes(typeof(T), true);
            return atts.Length > 0;
        }
    }
}