using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using System.Xml;

namespace ACBrLib
{
    public static class UiExtensions
    {
        public static void LoadXml(this WebBrowser browser, string xml)
        {
            if (xml == null)
                return;

            var path = Path.GetTempPath();
            var fileName = Guid.NewGuid() + ".xml";
            var fullFileName = Path.Combine(path, fileName);
            var xmlDoc = new XmlDocument();
            if (File.Exists(xml))
                xmlDoc.Load(xml);
            else
                xmlDoc.LoadXml(xml);
            xmlDoc.Save(fullFileName);
            browser.Navigate(fullFileName);
        }

        public static void DataSource<T>(this ComboBox cmb) where T : struct
        {
            cmb.DataSource = (from T value in Enum.GetValues(typeof(T)) select new ItemData<T>(value)).ToList();
        }

        public static void DataSource<T>(this ComboBox cmb, T valorPadrao) where T : struct
        {
            var list = (from T value in Enum.GetValues(typeof(T)) select new ItemData<T>(value)).ToList();
            cmb.DataSource = list;
            cmb.SelectedItem = list.SingleOrDefault(x => x.Content.Equals(valorPadrao));
        }

        public static T SelectedValue<T>(this ComboBox cmb) where T : struct
        {
            return ((ItemData<T>)cmb.SelectedItem).Content;
        }

        public static void SelectedValue<T>(this ComboBox cmb, T value) where T : struct
        {
            var list = (List<ItemData<T>>)cmb.DataSource;
            cmb.SelectedItem = list.Find(x => x.Content.Equals(value));
        }
    }
}