using System;
using System.Drawing.Printing;
using System.Globalization;
using System.IO;
using System.IO.Ports;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.BAL;
using ACBrLib.Core;
using ACBrLib.Core.BAL;
using ACBrLib.Core.Serial;

namespace ACBrLibBal.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrBAL bal;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            bal = new ACBrBAL();
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            bal.Dispose();
            bal = null;
        }

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            cmbModelo.EnumDataSource(ACBrBALModelo.balNenhum);

            cmbPorta.Items.AddRange(SerialPort.GetPortNames());

            cmbPorta.Items.Add("LPT1");
            cmbPorta.Items.Add("LPT2");
            cmbPorta.Items.Add(@"\\localhost\Epson");
            cmbPorta.Items.Add(@"c:\temp\ecf.txt");
            cmbPorta.Items.Add("TCP:192.168.0.31:9100");

            cmbBaud.EnumDataSource(SerialBaud.bd110);
            cmbDatabits.EnumDataSource(SerialDataBits.db5);
            cmbStopbits.EnumDataSource(SerialStopBytes.One);
            cmbParity.EnumDataSource(SerialParity.None);
            cmbHandshaking.EnumDataSource(SerialHandShake.Nenhum);

            // Altera as config de log
            bal.ConfigGravarValor(ACBrSessao.Principal, "LogNivel", 4);

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            bal.ConfigGravarValor(ACBrSessao.Principal, "LogPath", logPath);
            bal.ConfigGravar();

            LoadConfig();
        }

        #endregion Constructors

        #region Methods

        private void SalvarConfig()
        {
            bal.Config.Modelo = cmbModelo.GetSelectedValue<ACBrBALModelo>();
            bal.Config.Porta = cmbPorta.Text;
            bal.Config.Device.Baud = cmbBaud.GetSelectedValue<SerialBaud>();
            bal.Config.Device.Data = cmbDatabits.GetSelectedValue<SerialDataBits>();
            bal.Config.Device.Parity = cmbParity.GetSelectedValue<SerialParity>();
            bal.Config.Device.Stop = cmbStopbits.GetSelectedValue<SerialStopBytes>();
            bal.Config.Device.HandShake = cmbHandshaking.GetSelectedValue<SerialHandShake>();
            bal.Config.Device.MaxBandwidth = (int)nudMaxBand.Value;
            bal.Config.Device.SendBytesCount = (int)nudBytesCount.Value;
            bal.Config.Device.SendBytesInterval = (int)nudIntervalo.Value;
            bal.Config.Device.SoftFlow = chkSoftFlow.Checked;
            bal.Config.Device.HardFlow = chkHardFlow.Checked;

            bal.ConfigGravar();
        }

        private void LoadConfig()
        {
            bal.ConfigLer();

            cmbModelo.SetSelectedValue(bal.Config.Modelo);
            cmbPorta.Text = bal.Config.Porta;
            cmbBaud.SetSelectedValue(bal.Config.Device.Baud);
            cmbDatabits.SetSelectedValue(bal.Config.Device.Data);
            cmbParity.SetSelectedValue(bal.Config.Device.Parity);
            cmbStopbits.SetSelectedValue(bal.Config.Device.Stop);
            cmbHandshaking.SetSelectedValue(bal.Config.Device.HandShake);
            nudMaxBand.Value = bal.Config.Device.MaxBandwidth;
            nudBytesCount.Value = bal.Config.Device.SendBytesCount;
            nudIntervalo.Value = bal.Config.Device.SendBytesInterval;
            chkSoftFlow.Checked = bal.Config.Device.SoftFlow;
            chkHardFlow.Checked = bal.Config.Device.HardFlow;
        }

        #endregion Methods

        #region Eventhandlers

        private void btnAtivar_Click(object sender, EventArgs e)
        {
            try
            {
                if (btnAtivar.Text == @"Ativar")
                {
                    SalvarConfig();
                    bal.Ativar();
                    btnAtivar.Text = @"Desativar";
                }
                else
                {
                    bal.Desativar();
                    btnAtivar.Text = @"Ativar";
                }
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLerPeso_Click(object sender, EventArgs e)
        {
            try
            {
                var peso = bal.LePeso();
                txtPesoLido.Text = peso.ToString(CultureInfo.InvariantCulture);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSolicitarPeso_Click(object sender, EventArgs e)
        {
            try
            {
                bal.SolicitarPeso();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnUlitmoPeso_Click(object sender, EventArgs e)
        {
            try
            {
                var peso = bal.UltimoPesoLido();
                txtPesoLido.Text = peso.ToString(CultureInfo.InvariantCulture);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        #endregion Eventhandlers
    }
}