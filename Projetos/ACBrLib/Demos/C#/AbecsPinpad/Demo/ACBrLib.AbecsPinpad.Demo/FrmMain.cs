using System;
using System.Drawing;
using System.Drawing.Printing;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Windows.Forms;
using System.Xml.Linq;
using ACBrLib;
using ACBrLib.AbecsPinpad;
using ACBrLib.Core;
using ACBrLib.Core.AbecsPinpad;

namespace ACBrLibAbecsPinpad.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrAbecsPinpad AbecsPinpad;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            AbecsPinpad = new ACBrAbecsPinpad();
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbMsgAlign.EnumDataSource(MsgAlign.alCenter);
            ckbWordWrap.Enabled = true;

            cmbPortasCOMPinpad.Items.AddRange(SerialPort.GetPortNames());

            // Altera as config de log
            AbecsPinpad.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);
            AbecsPinpad.ConfigGravarValor(ACBrSessao.Principal, "LogPath", Path.Combine(Application.StartupPath, "Docs"));
            AbecsPinpad.ConfigGravar();

            LoadConfig();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll
            AbecsPinpad.Dispose();
        }

        private void btnSalvarConfiguracoes_Click(object sender, EventArgs e)
        {
            try
            { 
                SaveConfig();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void btnAtivar_Click(object sender, EventArgs e)
        {
            ToogleActivate();
        }

        private void btnSerial_Click(object sender, EventArgs e)
        {
            using (var form = new FrmSerial())
            {
                form.AbecsPinpad = AbecsPinpad;

                if (form.ShowDialog(this) != DialogResult.OK) return;

                cmbPortasCOMPinpad.SelectedItem = AbecsPinpad.Config.PortaPinpad;
            }
        }

        private void btnArqLog_Click(object sender, EventArgs e)
        {
            txtArqLogPinpad.Text = Helpers.OpenFile("Arquivo Log (*.log)|*.log|Todo os Arquivos (*.*)|*.*", checkFileExists: false);
        }


        #endregion EventHandlers

        private void SaveConfig()
        {
            AbecsPinpad.Config.PortaPinpad = cmbPortasCOMPinpad.Text;
            AbecsPinpad.Config.TimeOut = (int)nudTimeOutPinpad.Value;
            AbecsPinpad.Config.LogFile = txtArqLogPinpad.Text;
            AbecsPinpad.Config.MsgAlign = cmbMsgAlign.GetSelectedValue<MsgAlign>();
            AbecsPinpad.Config.MsgWordWrap = ckbWordWrap.Checked;
            AbecsPinpad.ConfigGravar();
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            AbecsPinpad.ConfigLer(file);

            nudTimeOutPinpad.Value = AbecsPinpad.Config.TimeOut;
            txtArqLogPinpad.Text = AbecsPinpad.Config.LogFile;
            cmbMsgAlign.SetSelectedValue(AbecsPinpad.Config.MsgAlign);
            ckbWordWrap.Checked = AbecsPinpad.Config.MsgWordWrap;
        }

        private void ToogleActivate()
        {
            if (btnSerial.Enabled)
            {
                AbecsPinpad.Ativar();
                btnSerial.Enabled = false;
                btnAtivar.Text = @"Desativar";
            }
            else
            {
                AbecsPinpad.Desativar();
                btnSerial.Enabled = true;
                btnAtivar.Text = @"Ativar";
            }
        }

        private void btnOPN_Click(object sender, EventArgs e)
        {
            AbecsPinpad.OPN();
        }
        private void btnCLO_Click(object sender, EventArgs e)
        {
            AbecsPinpad.CLO(txtCLOLinha1.Text + "\r\n" + txtCLOLinha2.Text);
        }

        private void btnCLXMensagem_Click(object sender, EventArgs e)
        {
            AbecsPinpad.CLX(rtbCLXMensagem.Text);
        }

        private void btnCLXImagem_Click(object sender, EventArgs e)
        {
            var sMensagemOuNomeImagem = "";
            if (InputBox.Show("Comando CLX", "Informe Nome da Imagem:", ref sMensagemOuNomeImagem) != DialogResult.OK) return;

            AbecsPinpad.CLX(sMensagemOuNomeImagem);
        }

        private void btnGIX_Click(object sender, EventArgs e)
        {
            try
            {
                var PPDATA = "";
                if (InputBox.Show("Comando GIX", "Informe PPDATA:", ref PPDATA) != DialogResult.OK) return;

                var ret = AbecsPinpad.GIX(PPDATA);
                rtbRespostaGIX.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnPinPadCapabilities_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = AbecsPinpad.PinPadCapabilities();
                rtbRespostaPinPadCapabilities.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGIN_Click(object sender, EventArgs e)
        {
            try
            {
                var ACQIDX = nudACQIDX.Value;
                int GIN_ACQIDX = Decimal.ToInt32(ACQIDX);
                var ret = AbecsPinpad.GIN(GIN_ACQIDX);
                rtbRespostaGIN.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDSP_Click(object sender, EventArgs e)
        {
            AbecsPinpad.DSP(txtLinha1.Text+"\r\n"+txtLinha2.Text);
        }

        private void btnClearDSP_Click(object sender, EventArgs e)
        {
            AbecsPinpad.DSP("");
        }

        private void btnDEX_Click(object sender, EventArgs e)
        {
            AbecsPinpad.DEX(rtbDEXMensagem.Text);
        }

        private void btnClearDEX_Click(object sender, EventArgs e)
        {
            AbecsPinpad.DEX("");
        }

        private void btnCarregarImagem_Click(object sender, EventArgs e)
        {

            var imagem = Helpers.OpenFile("Image files (*.png, *.jpg, *.gif) | *.png; *.jpg; *.gif");
            if (string.IsNullOrEmpty(imagem)) return;

            Image img = Image.FromFile(imagem);
            pictureBox1.Image = img;
            txtPathImagem.Text = imagem;
        }

        private void btnLoadMedia_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = AbecsPinpad.LoadMedia(txtPathImagem.Text, cmbTipoImagem.TabIndex);
                rtbRespostaLoadMedia.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLMF_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = AbecsPinpad.LMF();
                rtbRespostaLMF.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDSI_Click(object sender, EventArgs e)
        {
            try
            {
                var NomeArquivo = "";
                if (InputBox.Show("Comando DSI", "Informe Nome da Imagem:", ref NomeArquivo) != DialogResult.OK) return;

                AbecsPinpad.DSI(NomeArquivo);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnDMF_Click(object sender, EventArgs e)
        {
            try
            {
                var NomeArquivo = "";
                if (InputBox.Show("Comando DMF", "Informe Nome da Imagem:", ref NomeArquivo) != DialogResult.OK) return;

                AbecsPinpad.DMF(NomeArquivo);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGKY_Click(object sender, EventArgs e)
        {
            try
            {
                AbecsPinpad.GKY();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnRMC_Click(object sender, EventArgs e)
        {
            AbecsPinpad.RMC(txtRMCLinha1.Text + "\r\n" + txtRMCLinha2.Text);
        }

        private void btnGCD_Click(object sender, EventArgs e)
        {
            try
            {
                var timeout = nudTimeoutGCD.Value;
                int timeoutGCD = Decimal.ToInt32(timeout);

                var ret = AbecsPinpad.GCD(cmbMsgIndex.SelectedIndex, timeoutGCD);
                rtbRespostaGCD.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnMNU_Click(object sender, EventArgs e)
        {
            try
            {
                var timeout = nudTimeOutMNU.Value;
                int timeoutMNU = Decimal.ToInt32(timeout);

                var ret = AbecsPinpad.MNU(rtbOpcoesMenu.Text, txtTituloMNU.Text, timeoutMNU);
                rtbRespostaMNU.AppendText(ret.ToString());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCEX_Click(object sender, EventArgs e)
        {
            try
            {
                var timeout = nudTimeOutCEX.Value;
                int timeoutCEX = Decimal.ToInt32(timeout);
                if (ckbVerifyKey.Checked == true)
                {
                    AbecsPinpad.DEX("Press Key");
                    var ret = AbecsPinpad.CEX(true, false, false, false, false, timeoutCEX);
                    rtbRespostaCEX.AppendText(ret.ToString());
                    AbecsPinpad.DEX("");
                } 
                else if (ckbVerifyMagnetic.Checked == true)
                {
                    AbecsPinpad.DEX("Swipe the card");
                    var ret = AbecsPinpad.CEX(false, true, false, false, false, timeoutCEX);
                    rtbRespostaCEX.AppendText(ret.ToString());
                    AbecsPinpad.DEX("");
                }
                else if (ckbVerifyICCInsertion.Checked == true)
                {
                    AbecsPinpad.DEX("Insert card");
                    var ret = AbecsPinpad.CEX(false, false, true, false, false, timeoutCEX);
                    rtbRespostaCEX.AppendText(ret.ToString());
                    AbecsPinpad.DEX("");
                }
                else if (ckbVerifyICCRemoval.Checked == true)
                {
                    AbecsPinpad.DEX("Remove card");
                    var ret = AbecsPinpad.CEX(false, false, false, true, false, timeoutCEX);
                    rtbRespostaCEX.AppendText(ret.ToString());
                    AbecsPinpad.DEX("");
                }
                else if (ckbVerifyCTLSPresence.Checked == true)
                {
                    AbecsPinpad.DEX("Bring card closer");
                    var ret = AbecsPinpad.CEX(false, false, false, false, true, timeoutCEX);
                    rtbRespostaCEX.AppendText(ret.ToString());
                    AbecsPinpad.DEX("");
                }
            }
            catch(Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion Methods
    }
}