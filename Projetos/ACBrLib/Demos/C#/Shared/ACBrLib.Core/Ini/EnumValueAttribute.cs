using System;

namespace ACBrLib.Core
{
    [AttributeUsage(AttributeTargets.Field)]
    public sealed class EnumValueAttribute : Attribute
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="EnumValueAttribute"/> class.
        /// </summary>
        /// <param name="value">The value.</param>
        public EnumValueAttribute(string value)
        {
            Value = value;
        }

        /// <summary>
        /// Gets or sets the value.
        /// </summary>
        /// <value>The value.</value>
        public string Value { get; set; }
    }
}