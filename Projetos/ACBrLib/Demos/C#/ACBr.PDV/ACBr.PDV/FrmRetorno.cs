using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace ACBr.PDV
{
    public partial class FrmRetorno : Form
    {
        public FrmRetorno()
        {
            InitializeComponent();
        }

        public static void ShowRetorno(Form parent, string retorno)
        {
            var form = new FrmRetorno();
            form.rtbLog.AppendText(retorno);
            form.ShowDialog(parent);
        }
    }
}
