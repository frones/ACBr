using System;

namespace ACBrLib.Core
{
    [AttributeUsage(AttributeTargets.Property)]
    public sealed class IniKeyAttribute : Attribute
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="IniKeyAttribute"/> class.
        /// </summary>
        /// <param name="value">The value.</param>
        public IniKeyAttribute(string value)
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