using System;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;

namespace ACBrLib
{
    public static class InputBox
    {
        public static DialogResult Show(string title, string promptText, ref string value)
        {
            return Show<string>(title, promptText, ref value);
        }

        public static DialogResult Show<T>(string title, string promptText, ref T value)
        {
            var form = new Form();
            var label = new Label();
            var textBox = new TextBox();
            var buttonOk = new Button();
            var buttonCancel = new Button();

            form.Text = title;
            label.Text = promptText;
            textBox.Text = value.ToString();

            buttonOk.Text = @"OK";
            buttonCancel.Text = @"Cancelar";
            buttonOk.DialogResult = DialogResult.OK;
            buttonCancel.DialogResult = DialogResult.Cancel;

            var graph = form.CreateGraphics();
            var size = graph.MeasureString(promptText, label.Font);

            label.SetBounds(9, 5 + (int)size.Height, 372, 20);
            textBox.SetBounds(12, 30 + (int)size.Height, 372, 20);
            buttonOk.SetBounds(228, 72, 75, 23);
            buttonCancel.SetBounds(309, 72, 75, 23);

            label.AutoSize = true;
            textBox.Anchor = textBox.Anchor | AnchorStyles.Right;
            buttonOk.Anchor = AnchorStyles.Bottom | AnchorStyles.Right;
            buttonCancel.Anchor = AnchorStyles.Bottom | AnchorStyles.Right;

            form.ClientSize = new Size(396, 107);
            form.Controls.AddRange(new Control[] { label, textBox, buttonOk, buttonCancel });
            form.ClientSize = new Size(Math.Max(300, label.Right + 10), form.ClientSize.Height + (int)size.Height);
            form.FormBorderStyle = FormBorderStyle.FixedDialog;
            form.StartPosition = FormStartPosition.CenterScreen;
            form.MinimizeBox = false;
            form.MaximizeBox = false;
            form.AcceptButton = buttonOk;
            form.CancelButton = buttonCancel;

            var dialogResult = form.ShowDialog();

            var type = typeof(T);

            try
            {
                if (type.IsEnum || type.IsGenericType && type.GetGenericArguments()[0].IsEnum)
                {
                    value = (T)Enum.Parse(type, textBox.Text);
                }
                else
                {
                    value = (T)Convert.ChangeType(textBox.Text, type, CultureInfo.InvariantCulture);
                }
            }
            catch (Exception)
            {
                value = default(T);
            }

            return dialogResult;
        }
    }
}