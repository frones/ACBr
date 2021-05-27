using System;
using System.Collections.Generic;
using System.Windows.Forms;
using ACBr.PDV.Model;
using ACBrLib;
using ACBrLib.NFe;

namespace ACBr.PDV
{
    public partial class FrmPagamento : Form
    {
        #region Fields

        private List<ItemData<Pagamento>> Tipos;
        private CaixaPDV caixa;

        #endregion Fields

        #region Constuctors

        public FrmPagamento()
        {
            InitializeComponent();
        }

        private void FrmPagamento_Load(object sender, EventArgs e)
        {
            Tipos = new List<ItemData<Pagamento>>
            {
                new ItemData<Pagamento>("Dinheiro", new Pagamento {Descricao = "Dinheiro", TipoNFe = FormaPagamento.fpDinheiro, TipoSAT = 1}),
                new ItemData<Pagamento>("Cartão Debito", new Pagamento {Descricao = "Cartão Debito", TipoNFe = FormaPagamento.fpCartaoDebito, TipoSAT = 3}),
                new ItemData<Pagamento>("Cartão de Credito", new Pagamento {Descricao = "Cartão de Credito", TipoNFe = FormaPagamento.fpCartaoCredito, TipoSAT = 4}),
                new ItemData<Pagamento>("Credito Loja", new Pagamento {Descricao = "Credito Loja", TipoNFe = FormaPagamento.fpCreditoLoja, TipoSAT = 5}),
                new ItemData<Pagamento>("Vale Presente", new Pagamento {Descricao = "Vale Presente", TipoNFe = FormaPagamento.fpValePresente, TipoSAT = 12}),
                new ItemData<Pagamento>("Outros", new Pagamento {Descricao = "Outros", TipoNFe = FormaPagamento.fpOutro, TipoSAT = 99})
            };

            cmbTipoPagamento.DataSource = Tipos;
        }

        #endregion Constuctors

        #region Methods

        public static void EfetuarPagamento(Form parent, CaixaPDV caixa)
        {
            var form = new FrmPagamento { caixa = caixa };
            form.ShowDialog(parent);
        }

        #endregion Methods

        #region Event Handlers

        private void btnCancelar_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void btnVender_Click(object sender, EventArgs e)
        {
            var pagamento = cmbTipoPagamento.GetSelectedValue<Pagamento>();
            pagamento.Valor = nudValor.Value;
            caixa.EfetuarPagamento(pagamento);
            Close();
        }

        #endregion Event Handlers
    }
}