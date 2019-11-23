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

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cmbPorta.Items.Add($"RAW:{printer}");
            }

            cmbBaud.Items.Add(1200);
            cmbBaud.Items.Add(2400);
            cmbBaud.Items.Add(4800);
            cmbBaud.Items.Add(9600);
            cmbBaud.Items.Add(19200);
            cmbBaud.Items.Add(38400);
            cmbBaud.Items.Add(57600);
            cmbBaud.Items.Add(115200);

            cmbDatabits.Items.Add(5);
            cmbDatabits.Items.Add(6);
            cmbDatabits.Items.Add(7);
            cmbDatabits.Items.Add(8);

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
            bal.ConfigGravarValor(ACBrSessao.BAL, "Modelo", cmbModelo.GetSelectedValue<ACBrBALModelo>());
            bal.ConfigGravarValor(ACBrSessao.BAL, "Porta", cmbPorta.Text);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Baud", cmbBaud.SelectedItem);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Data", cmbDatabits.SelectedItem);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Parity", cmbParity.SelectedItem);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "Stop", cmbStopbits.SelectedItem);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "HandShake", cmbHandshaking.SelectedItem);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "MaxBandwidth", nudMaxBand.Value);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "SendBytesCount", nudBytesCount.Value);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "SendBytesInterval", nudIntervalo.Value);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "SoftFlow", chkSoftFlow.Checked);
            bal.ConfigGravarValor(ACBrSessao.BAL_Device, "HardFlow", chkHardFlow.Checked);

            bal.ConfigGravar();
        }

        private void LoadConfig()
        {
            bal.ConfigLer();

            cmbModelo.SetSelectedValue(bal.ConfigLerValor<ACBrBALModelo>(ACBrSessao.BAL, "Modelo"));
            cmbPorta.Text = bal.ConfigLerValor<string>(ACBrSessao.BAL, "Porta");
            cmbBaud.SelectedItem = bal.ConfigLerValor<int>(ACBrSessao.BAL_Device, "Baud");
            cmbDatabits.SelectedItem = bal.ConfigLerValor<int>(ACBrSessao.BAL_Device, "Data");
            cmbParity.SelectedItem = bal.ConfigLerValor<SerialParity>(ACBrSessao.BAL_Device, "Parity");
            cmbStopbits.SelectedItem = bal.ConfigLerValor<SerialStopBytes>(ACBrSessao.BAL_Device, "Stop");
            cmbHandshaking.SelectedItem = bal.ConfigLerValor<SerialHandShake>(ACBrSessao.BAL_Device, "HandShake");
            nudMaxBand.Value = bal.ConfigLerValor<decimal>(ACBrSessao.BAL_Device, "MaxBandwidth");
            nudBytesCount.Value = bal.ConfigLerValor<decimal>(ACBrSessao.BAL_Device, "SendBytesCount");
            nudIntervalo.Value = bal.ConfigLerValor<decimal>(ACBrSessao.BAL_Device, "SendBytesInterval");
            chkSoftFlow.Checked = bal.ConfigLerValor<bool>(ACBrSessao.BAL_Device, "SoftFlow");
            chkHardFlow.Checked = bal.ConfigLerValor<bool>(ACBrSessao.BAL_Device, "HardFlow");
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