using System.ComponentModel;
using System.Linq;
using System.Reflection;

namespace ACBrLib
{
    public sealed class ItemData<T> where T : struct
    {
        #region Constructors

        public ItemData()
        {
        }

        public ItemData(string description, T content)
        {
            Description = description;
            Content = content;
        }

        public ItemData(T value)
        {
            Content = value;
            if (typeof(T).IsEnum)
            {
                var fi = typeof(T).GetField(value.ToString(), BindingFlags.Static | BindingFlags.Public);
                var att = fi?.GetCustomAttributes(typeof(DescriptionAttribute), true).FirstOrDefault() as DescriptionAttribute;

                Description = att?.Description ?? value.ToString();
            }
            else
            {
                Description = value.ToString();
            }
        }

        #endregion Constructors

        #region Properties

        public string Description { get; set; }

        public T Content { get; set; }

        #endregion Properties

        #region Methods

        public override string ToString()
        {
            return Description;
        }

        #endregion Methods
    }
}