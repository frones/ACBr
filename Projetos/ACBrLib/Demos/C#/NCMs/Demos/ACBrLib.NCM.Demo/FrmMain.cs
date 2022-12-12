using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.NCM;
using System.Linq;

namespace ACBrLibNCM.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrNCM ACBrNCM;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrNCM = new ACBrNCM();
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando...");

            try
            {
                // Altera as config de log
                ACBrNCM.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrNCM.Config.Principal.LogPath = logPath;
                ACBrNCM.ConfigGravar();

            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll
            ACBrNCM.Dispose();
        }

        #endregion EventHandlers

        private void btnObterNCMs_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrNCM.ObterNCMs());
            } 
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnBaixarLista_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrNCM.BaixarLista("C:\\temp\\NCMs.txt"));
            } 
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnFiltrarPorCodigo_Click(object sender, EventArgs e)
        {
            try 
            { 
                rtbRespostas.AppendText(ACBrNCM.BuscarPorCodigo(txtFiltrarPorCodigo.Text)); 
            } 
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnFiltrarPorDescricao_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrNCM.BuscarPorDescricao(txtFiltrarPorDescricao.Text, cmbFiltroPorDescricao.SelectedIndex));
            } 
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnValidarNCM_Click(object sender, EventArgs e)
        {
            try
            {
                rtbRespostas.AppendText(ACBrNCM.Validar(txtValidarNCM.Text));
            } 
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion Methods       
    }
}