using System;
using System.Drawing.Printing;
using System.IO.Ports;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core.Config;
using ACBrLib.Core.Serial;
using ACBrLib.PosPrinter;

namespace ACBrLibPosPrinter.Demo
{
    public partial class FrmSerial : Form
    {
        #region Fields

        public ACBrPosPrinter PosPrinter { get; set; }

        #endregion Fields

        #region Constructor

        public FrmSerial()
        {
            InitializeComponent();
        }

        private void SerialCFGForm_Load(object sender, EventArgs e)
        {
            Inicializar();
        }

        #endregion Constructor

        #region Methods

        private void Inicializar()
        {
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

            cmbPorta.SelectedItem = PosPrinter.Config.Porta;

            cmbVelocidade.EnumDataSource(PosPrinter.Config.Device.Baud);
            cmbDatabits.EnumDataSource(PosPrinter.Config.Device.Data);
            cmbStopbits.EnumDataSource(PosPrinter.Config.Device.Stop);
            cmbParity.EnumDataSource(PosPrinter.Config.Device.Parity);
            cmbHandshaking.EnumDataSource(PosPrinter.Config.Device.HandShake);

            timeOutNumericUpDown.Value = PosPrinter.Config.Device.TimeOut;
            chkHardFlow.Checked = PosPrinter.Config.Device.HardFlow;
            chkSoftFlow.Checked = PosPrinter.Config.Device.SoftFlow;
        }

        #endregion Methods

        #region EventHandler

        private void btnConfirmar_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void cmbPorta_SelectedIndexChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Porta = (string)cmbPorta.SelectedItem;
        }

        private void cmbVelocidade_SelectedIndexChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.Baud = cmbVelocidade.GetSelectedValue<SerialBaud>();
        }

        private void timeOutNumericUpDown_ValueChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.TimeOut = (int)timeOutNumericUpDown.Value;
        }

        private void cmbDatabits_SelectedIndexChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.Data = cmbDatabits.GetSelectedValue<SerialDataBits>();
        }

        private void cmbStopbits_SelectedIndexChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.Stop = cmbStopbits.GetSelectedValue<SerialStopBytes>();
        }

        private void cmbParity_SelectedIndexChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.Parity = cmbParity.GetSelectedValue<SerialParity>();
        }

        private void cmbHandshaking_SelectedIndexChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.HandShake = cmbHandshaking.GetSelectedValue<SerialHandShake>();
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.HardFlow = chkHardFlow.Checked;
        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            PosPrinter.Config.Device.SoftFlow = chkSoftFlow.Checked;
        }

        #endregion EventHandler
    }
}