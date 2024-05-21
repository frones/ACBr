using System;
using System.Drawing.Printing;
using System.IO.Ports;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.AbecsPinpad;
using ACBrLib.Core.Serial;
using ACBrLibAbecsPinpad;

namespace ACBrLibAbecsPinpad.Demo
{
    public partial class FrmSerial : Form
    {
        #region Fields

        public ACBrAbecsPinpad AbecsPinpad { get; set; }

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
            cmbPorta.SelectedItem = AbecsPinpad.Config.PortaPinpad;

            cmbVelocidade.EnumDataSource(AbecsPinpad.Config.Device.Baud);
            cmbDatabits.EnumDataSource(AbecsPinpad.Config.Device.Data);
            cmbStopbits.EnumDataSource(AbecsPinpad.Config.Device.Stop);
            cmbParity.EnumDataSource(AbecsPinpad.Config.Device.Parity);
            cmbHandshaking.EnumDataSource(AbecsPinpad.Config.Device.HandShake);

            timeOutNumericUpDown.Value = AbecsPinpad.Config.Device.TimeOut;
            chkHardFlow.Checked = AbecsPinpad.Config.Device.HardFlow;
            chkSoftFlow.Checked = AbecsPinpad.Config.Device.SoftFlow;
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
            AbecsPinpad.Config.PortaPinpad = (string)cmbPorta.SelectedItem;
        }

        private void cmbVelocidade_SelectedIndexChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.Baud = cmbVelocidade.GetSelectedValue<SerialBaud>();
        }

        private void timeOutNumericUpDown_ValueChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.TimeOut = (int)timeOutNumericUpDown.Value;
        }

        private void cmbDatabits_SelectedIndexChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.Data = cmbDatabits.GetSelectedValue<SerialDataBits>();
        }

        private void cmbStopbits_SelectedIndexChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.Stop = cmbStopbits.GetSelectedValue<SerialStopBytes>();
        }

        private void cmbParity_SelectedIndexChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.Parity = cmbParity.GetSelectedValue<SerialParity>();
        }

        private void cmbHandshaking_SelectedIndexChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.HandShake = cmbHandshaking.GetSelectedValue<SerialHandShake>();
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.HardFlow = chkHardFlow.Checked;
        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            AbecsPinpad.Config.Device.SoftFlow = chkSoftFlow.Checked;
        }

        #endregion EventHandler
    }
}