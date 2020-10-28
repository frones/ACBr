using System;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace ACBrLib
{
    public static class UIExtensions
    {
        public static void EnumDataSource<T>(this ComboBox cmb) where T : struct
        {
            cmb.DataSource = (from T value in Enum.GetValues(typeof(T)) select new ItemData<T>(value))
                .OrderBy(x => x.Description).ToArray();
        }

        public static void EnumDataSource<T>(this ComboBox cmb, T valorPadrao) where T : struct
        {
            var dataSource = (from T value in Enum.GetValues(typeof(T)) select new ItemData<T>(value))
                .OrderBy(x => x.Description).ToArray();
            cmb.DataSource = dataSource;
            cmb.SelectedItem = dataSource.SingleOrDefault(x => x.Content.Equals(valorPadrao));
        }

        public static void EnumDataSource<T>(this ComboBox cmb, T valorPadrao, params T[] excluded) where T : struct
        {
            var dataSource = (from T value in Enum.GetValues(typeof(T)) where !excluded.Contains(value) select new ItemData<T>(value))
                .OrderBy(x => x.Description).ToArray();
            cmb.DataSource = dataSource;
            cmb.SelectedItem = dataSource.SingleOrDefault(x => x.Content.Equals(valorPadrao));
        }

        public static void SetDataSource<T>(this ComboBox cmb, T valorPadrao)
        {
        }

        public static T GetSelectedValue<T>(this ComboBox cmb)
        {
            return ((ItemData<T>)cmb.SelectedItem).Content;
        }

        public static void SetSelectedValue<T>(this ComboBox cmb, T valor)
        {
            var dataSource = (ItemData<T>[])cmb.DataSource;
            cmb.SelectedItem = dataSource.SingleOrDefault(x => x.Content.Equals(valor));
        }

        public static void AppendLine(this RichTextBox source, string value)
        {
            if (source.Text.Length == 0)
                source.Text = value;
            else
                source.AppendText(Environment.NewLine + value);
        }

        public static void AppendLine(this RichTextBox source, string[] values)
        {
            foreach (var value in values)
            {
                source.AppendLine(value);
            }
        }

        public static void AppendLine(this TextBox source, string value)
        {
            if (source.Text.Length == 0)
                source.Text = value;
            else
                source.AppendText(Environment.NewLine + value);
        }

        public static void AppendLine(this TextBox source, string[] values)
        {
            foreach (var value in values)
            {
                source.AppendLine(value);
            }
        }

        public static void HideTabHeaders(this TabControl tabControl)
        {
            tabControl.Appearance = TabAppearance.FlatButtons;
            tabControl.ItemSize = new Size(0, 1);
            tabControl.SizeMode = TabSizeMode.Fixed;

            tabControl.SelectedTab = tabControl.TabPages[0];
        }
    }
}