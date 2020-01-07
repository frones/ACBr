using System;
using System.ComponentModel;

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
            Description = value.ToString();

            if (!(value is Enum)) return;

            var fieldInfo = value.GetType().GetField(value.ToString());
            var atributos = (DescriptionAttribute[])fieldInfo.GetCustomAttributes(typeof(DescriptionAttribute), false);

            Description = atributos.Length > 0 ? atributos[0].Description ?? "Nulo" : value.ToString();
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