using System;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;
using ACBr.PDV.Model;
using ACBrLib;

namespace ACBr.PDV
{
    public partial class FrmVenderItem : Form
    {
        #region Fields

        private CaixaPDV caixa;

        #endregion Fields

        #region Constructors

        public FrmVenderItem()
        {
            InitializeComponent();
        }

        #endregion Constructors

        #region Methods

        public static void VenderItem(Form parent, CaixaPDV caixa)
        {
            var form = new FrmVenderItem { caixa = caixa };
            form.ShowDialog(parent);
        }

        #endregion Methods

        #region Event Handlers

        private void btnBal_Click(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.Default.ShowInfo(SplashInfo.Message, "Aguarde Lendo Peso...");

            try
            {
                var peso = 0M;
                var tentativas = 0;

                caixa.Bal.Ativar();

                do
                {
                    tentativas++;
                    if (tentativas == 6)
                    {
                        SplashScreenManager.Close();
                        MessageBox.Show(@"Erro ao ler o peso da balança", @"Balança", MessageBoxButtons.OK, MessageBoxIcon.Error);
                        break;
                    }

                    peso = caixa.Bal.LePeso();

                    if (peso <= 0)
                        Thread.Sleep(500);
                } while (peso <= 0);

                if (peso > 0)
                    nudQuantidade.Value = peso;
            }
            finally
            {
                SplashScreenManager.Close();
                caixa.Bal.Desativar();
            }
        }

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void btnVender_Click(object sender, EventArgs e)
        {
            var produto = new Produto
            {
                Codigo = nudCodigo.Value.ToString(CultureInfo.InvariantCulture),
                Descricao = txtDescricao.Text,
                Unidade = txtUnidade.Text,
                Valor = nudValor.Value
            };

            caixa.VenderItem(produto, nudQuantidade.Value);
            Close();
        }

        #endregion Event Handlers
    }
}