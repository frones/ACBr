using System;
using System.Collections.Generic;
using System.Windows.Forms;
using ACBr.PDV.Model;
using ACBrLib;

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

        private void FrmPagamento_Load(object sender, System.EventArgs e)
        {
            Tipos = new List<ItemData<Pagamento>>
            {
                new ItemData<Pagamento>("Dinheiro", new Pagamento {Descricao = "Dinheiro", TipoNFe = 1, TipoSAT = 1}),
                new ItemData<Pagamento>("Cartão Debito", new Pagamento {Descricao = "Cartão Debito", TipoNFe = 3, TipoSAT = 3}),
                new ItemData<Pagamento>("Cartão de Credito", new Pagamento {Descricao = "Cartão de Credito", TipoNFe = 4, TipoSAT = 4}),
                new ItemData<Pagamento>("Credito Loja", new Pagamento {Descricao = "Credito Loja", TipoNFe = 5, TipoSAT = 5}),
                new ItemData<Pagamento>("Vale Presente", new Pagamento {Descricao = "Vale Presente", TipoNFe = 12, TipoSAT = 12}),
                new ItemData<Pagamento>("Outros", new Pagamento {Descricao = "Outros", TipoNFe = 99, TipoSAT = 99})
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