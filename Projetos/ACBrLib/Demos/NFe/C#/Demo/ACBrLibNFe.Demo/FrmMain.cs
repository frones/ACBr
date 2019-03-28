using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using ACBrLib;

namespace ACBrLibNFe.Demo
{
    public partial class FrmMain : Form
    {
        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a dll
            var ret = ACBrNFe.NFE_Inicializar("".ToUTF8(), "".ToUTF8());
            ACBrNFe.CheckResult(ret);
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbCrypt.SelectedIndex = 3;
            cmbHttp.SelectedIndex = 2;
            cmbXmlSign.SelectedIndex = 4;

            cmbUfDestino.SelectedItem = "SP";
            cmbSSlType.SelectedIndex = 0;

            // Altera as config de log
            var ret = ACBrNFe.NFE_ConfigGravarValor("Principal".ToUTF8(), "LogNivel".ToUTF8(), "4".ToUTF8());
            ACBrNFe.CheckResult(ret);

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            ret = ACBrNFe.NFE_ConfigGravarValor("Principal".ToUTF8(), "LogPath".ToUTF8(), logPath.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravar("".ToUTF8());
            ACBrNFe.CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            int ret;

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "SSLCryptLib".ToUTF8(), cmbCrypt.SelectedIndex.ToString().ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "SSLHttpLib".ToUTF8(), cmbHttp.SelectedIndex.ToString().ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "SSLXmlSignLib".ToUTF8(), cmbXmlSign.SelectedIndex.ToString().ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "ArquivoPFX".ToUTF8(), txtCertPath.Text.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "Senha".ToUTF8(), txtCertPassword.Text.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "NumeroSerie".ToUTF8(), txtCertNumero.Text.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "PathSchemas".ToUTF8(), txtSchemaPath.Text.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("DFe".ToUTF8(), "UF".ToUTF8(), cmbUfDestino.Text.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "Ambiente".ToUTF8(), $"{(rdbHomologacao.Checked ? "0" : "1")}".ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "SSLType".ToUTF8(), cmbSSlType.SelectedIndex.ToString().ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravarValor("NFe".ToUTF8(), "Timeout".ToUTF8(), nudTimeOut.Text.ToUTF8());
            ACBrNFe.CheckResult(ret);

            ret = ACBrNFe.NFE_ConfigGravar("".ToUTF8());
            ACBrNFe.CheckResult(ret);
        }

        #endregion Methods

        #region EventHandlers

        private void BtnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void BtnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void BtnSalvar_Click(object sender, EventArgs e)
        {
            SalvarConfig();
        }

        private void BtnStatusServ_Click(object sender, EventArgs e)
        {
            try
            {
                var bufferLen = 1024;
                var buffer = new StringBuilder(bufferLen);

                var ret = ACBrNFe.NFE_StatusServico(buffer, ref bufferLen);
                ACBrNFe.CheckResult(ret);

                if (bufferLen > 1024)
                {
                    ret = ACBrNFe.NFE_UltimoRetorno(buffer, ref bufferLen);
                    ACBrNFe.CheckResult(ret);
                }

                rtbRespostas.AppendText(buffer.FromUTF8());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion EventHandlers
    }
}