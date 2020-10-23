using System;
using System.Text;
using ACBr.Net.Core.Extensions;

namespace ACBr.PDV.Model
{
    public class RegistroVenda : RegistroBobina
    {
        #region Fields

        private int item;
        private decimal quantidade;
        private Produto produto;

        #endregion Fields

        #region Properties

        public Venda Venda { get; set; }

        public int Item
        {
            set
            {
                item = value;
                var sbLinha1 = new StringBuilder(Linha1);

                //Adiciona item ao registro na bobina
                var valor = "".PadLeft(4, '0') + value;
                valor = valor.Substring(valor.Length - 4);
                sbLinha1.Remove(0, 4);
                sbLinha1.Insert(0, valor);
                Linha1 = sbLinha1.ToString();
            }
            get => item;
        }

        public decimal Quantidade
        {
            set
            {
                quantidade = value;
                var sbLinha2 = new StringBuilder(Linha2);

                //Adiciona quantidade ao registro na bobina
                var valor = "".PadLeft(15, ' ') + value.ToString("N3");
                valor = valor.Substring(valor.Length - 8);
                sbLinha2.Remove(0, 8);
                sbLinha2.Insert(0, valor);
                Linha2 = sbLinha2.ToString();
            }
            get => quantidade;
        }

        public Produto Produto
        {
            set
            {
                var sbLinha1 = new StringBuilder(Linha1);
                var sbLinha2 = new StringBuilder(Linha2);

                produto = value;

                var codigo = "".PadLeft(30, ' ') + produto.Codigo;
                codigo = codigo.Substring(codigo.Length - 14);
                sbLinha1.Remove(5, 14);
                sbLinha1.Insert(5, codigo);

                var descricao = " " + produto.Descricao + "".PadLeft(30, ' ');
                descricao = descricao.Substring(0, 30);
                sbLinha1.Remove(19, 12);
                sbLinha1.Insert(19, descricao);

                var unidade = "".PadLeft(7, ' ') + produto.Unidade + " x ";
                unidade = unidade.Substring(unidade.Length - 7);
                sbLinha2.Remove(8, 7);
                sbLinha2.Insert(8, unidade);

                var valorVenda = "".PadLeft(14, ' ') + produto.Valor.ToString("N2");
                valorVenda = valorVenda.Substring(valorVenda.Length - 14);
                sbLinha2.Remove(15, 14);
                sbLinha2.Insert(15, valorVenda);

                var situacaoTributaria = "".PadLeft(5, ' ') + "ST";
                situacaoTributaria = situacaoTributaria.Substring(situacaoTributaria.Length - 4);
                sbLinha2.Remove(29, 4);
                sbLinha2.Insert(29, situacaoTributaria);

                var valorTotal = "".PadLeft(14, ' ') + ValorTotal.ToString("N2");
                valorTotal = valorTotal.Substring(valorTotal.Length - 14);
                sbLinha2.Remove(33, 14);
                sbLinha2.Insert(33, valorTotal);

                Linha1 = sbLinha1.ToString().Substring(0, 47);
                Linha2 = sbLinha2.ToString().Substring(0, 47);
            }

            get => produto;
        }

        public decimal ValorTotal => (produto.Valor * Quantidade).RoundABNT();

        public bool Cancelado { get; set; }

        #endregion Properties
    }
}