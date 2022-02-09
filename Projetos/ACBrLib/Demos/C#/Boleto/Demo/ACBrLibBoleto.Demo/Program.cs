using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using ACBrLib.Boleto;

namespace ACBrLibBoleto.Demo
{
    internal static class Program
    {
        private static ACBrBoleto bol;

        /// <summary>
        /// Ponto de entrada principal para o aplicativo.
        /// </summary>
        [STAThread]
        static void Main()
        {
            bol = new ACBrBoleto("[Memory]");

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new FrmMain());
        }
    }
}