using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using ACBr.Net.Core.Extensions;
using ACBr.PDV.Model;
using ACBrLib.BAL;
using ACBrLib.Core;
using ACBrLib.NFe;
using ACBrLib.PosPrinter;
using ACBrLib.Sat;

namespace ACBr.PDV
{
    public class CaixaPDV : IDisposable
    {
        #region Fields

        private readonly List<RegistroBobina> cabecalhoBobina;
        private readonly List<RegistroBobina> rodapeBobina;
        private readonly Venda VendaAtual;
        private string mensagemCaixa;
        private StatusVenda status;

        #endregion Fields

        #region Events

        public EventHandler<EventArgs> OnAtualizarBobina;
        public EventHandler<EventArgs> OnAtualizarMensagem;
        public EventHandler<EventArgs> OnStatusChange;
        public EventHandler<RetornoEventArgs> OnRetornoReceived;

        #endregion Events

        #region Constructors

        public CaixaPDV()
        {
            cabecalhoBobina = new List<RegistroBobina>();
            rodapeBobina = new List<RegistroBobina>();
            VendaAtual = new Venda();

            NFe = new ACBrNFe();

            // Altera as config de log, como todos os componentes
            // vão usar o mesmo ini então configuro so uma vez
            NFe.Config.Principal.LogNivel = NivelLog.logParanoico;

            var logPath = Path.Combine(Application.StartupPath, "Logs");
            if (!Directory.Exists(logPath))
                Directory.CreateDirectory(logPath);

            NFe.Config.Principal.LogPath = logPath;
            NFe.ConfigGravar();

            PosPrinter = new ACBrPosPrinter();
            SAT = new ACBrSat();
            Bal = new ACBrBAL();
        }

        #endregion Constructors

        #region Properties

        public ACBrNFe NFe { get; }

        public ACBrSat SAT { get; }

        public ACBrPosPrinter PosPrinter { get; }

        public ACBrBAL Bal { get; }

        public RegistroVenda[] Itens => VendaAtual.Items.Where(x => !x.Cancelado).ToArray();

        public Pagamento[] Pagamentos => VendaAtual.Pagamentos.ToArray();

        public decimal ValorVenda => VendaAtual.TotalVenda;

        public decimal ValorPago => VendaAtual.TotalPago;

        public decimal Troco => VendaAtual.Troco;

        public bool IsDisposed { get; private set; }

        public StatusVenda Status
        {
            get => status;
            private set
            {
                status = value;
                OnStatusChange.Raise(this);
            }
        }

        public string MensagemCaixa
        {
            get => mensagemCaixa;
            set
            {
                mensagemCaixa = value;
                OnAtualizarMensagem.Raise(this);
            }
        }

        public List<RegistroBobina> Bobina
        {
            get
            {
                var listaRegistrosAuxiliar = new List<RegistroBobina>(VendaAtual.Items.Concat(rodapeBobina));
                return new List<RegistroBobina>(cabecalhoBobina.Concat(listaRegistrosAuxiliar));
            }
        }

        #endregion Properties

        #region Venda

        public void IniciarVenda()
        {
            VendaAtual.Clear();
            cabecalhoBobina.Clear();
            rodapeBobina.Clear();

            MensagemCaixa = "Venda em Andamento";

            var separador = "".PadLeft(48, '-');
            var reg1 = new RegistroBobina(separador, "           ** DOCUMENTO NÃO FISCAL **           ");
            var reg2 = new RegistroBobina(separador, "ITEM CÓDIGO         DESCRIÇÃO                   ");
            var reg3 = new RegistroBobina("QTD.     UN      VL.UNIT.(R$) ST    VL.ITEM(R$)", separador);

            cabecalhoBobina.Add(reg1);
            cabecalhoBobina.Add(reg2);
            cabecalhoBobina.Add(reg3);

            //Passando numero da NFe e Serie
            VendaAtual.Serie = Configuracao.Instance.DFe.Serie;
            VendaAtual.Numero = Configuracao.Instance.DFe.NumeroAtual;

            rodapeBobina.Clear();
            Status = StatusVenda.Iniciada;
            OnAtualizarBobina.Raise(this);
        }

        public void CancelarVenda()
        {
            var separador = "".PadLeft(48, '-');

            var reg1 = new RegistroBobina(separador, "       ** DOCUMENTO NÃO FISCAL CANCELADO **     ");
            var reg2 = new RegistroBobina("                                                ", separador);

            rodapeBobina.Add(reg1);
            rodapeBobina.Add(reg2);

            MensagemCaixa = "Cupom NÃO fiscal cancelado. Caixa Livre.";
            Status = StatusVenda.Cancelada;
            OnAtualizarBobina.Raise(this);
        }

        public void Identificar(string documento, string nome = "")
        {
            if (string.IsNullOrEmpty(documento)) return;

            VendaAtual.Cliente.Documento = documento;
            VendaAtual.Cliente.Nome = nome;

            var cliente = string.IsNullOrEmpty(VendaAtual.Cliente.Nome) ? "Cliente" : VendaAtual.Cliente.Nome;

            MensagemCaixa = $@"{cliente} - {VendaAtual.Cliente.Documento}";
            OnAtualizarMensagem.Raise(this);
        }

        public void VenderItem(Produto produto, decimal quantidade)
        {
            var registro = new RegistroVenda
            {
                Venda = VendaAtual,
                Item = VendaAtual.Items.Count + 1,
                Quantidade = quantidade,
                Produto = produto
            };

            VendaAtual.Items.Add(registro);
            OnAtualizarBobina.Raise(this);
        }

        public void Cancelaritem(RegistroVenda item)
        {
            if (Itens.All(x => x.Produto.Codigo != item.Produto.Codigo)) return;

            item.Cancelado = true;
            item.Linha2 = "**** ITEM CANCELADO ****";
            OnAtualizarBobina.Raise(this);
        }

        public void EfetuarPagamento(Pagamento pagamento)
        {
            VendaAtual.Pagamentos.Add(pagamento);
            OnAtualizarBobina.Raise(this);
        }

        public void SubTotalizar()
        {
            Status = StatusVenda.EmPagamento;
            MensagemCaixa = "Em Pagamento";
            OnAtualizarBobina.Raise(this);
        }

        public bool EncerraCupom()
        {
            if (!VendaAtual.Pagamentos.Any()) return false;
            if (VendaAtual.TotalPago < VendaAtual.TotalVenda) return false;

            RetornoEventArgs retorno;
            switch (Configuracao.Instance.DFe.TipoDocumento)
            {
                case TipoDFe.NFCe:
                    retorno = VendaNFCe();
                    break;

                case TipoDFe.SAT:
                    retorno = VendaSAT();
                    break;

                default:
                    throw new ArgumentOutOfRangeException();
            }

            if (retorno.Sucesso)
            {
                var separador = "".PadLeft(48, '-');
                var aux = $"TOTAL R$: {VendaAtual.TotalVenda:N2}";
                var reg1 = new RegistroBobina(separador,
                    $"TOTAL R$: {"".PadLeft(48 - aux.Length, ' ')}{VendaAtual.TotalVenda:N2}");
                rodapeBobina.Add(reg1);

                foreach (var pag in VendaAtual.Pagamentos)
                {
                    var regPagamento = new RegistroBobina();
                    aux = $"{pag.Descricao} R$: {pag.Valor:N2}";
                    regPagamento.Linha1 = $"{pag.Descricao} R$: {"".PadLeft(48 - aux.Length, ' ')}{pag.Valor:N2}";
                    regPagamento.Linha2 = "";
                    rodapeBobina.Add(regPagamento);
                }

                aux = $"TOTAL RECEB. R$: {VendaAtual.TotalPago:N2}";
                var reg2 = new RegistroBobina(
                    $"TOTAL RECEB. R$: {"".PadLeft(48 - aux.Length, ' ')}{VendaAtual.TotalPago:N2}",
                    "TROCO R$:                        " + VendaAtual.Troco);

                aux = $"TROCO R$: {VendaAtual.Troco:N2}";
                reg2.Linha2 = $"TROCO R$: {"".PadLeft(48 - aux.Length, ' ')}{VendaAtual.Troco:N2}";
                var finalizador = new RegistroBobina(separador);

                rodapeBobina.Add(reg2);
                rodapeBobina.Add(finalizador);
                MensagemCaixa = "Caixa Livre";
                Status = StatusVenda.Livre;
                OnAtualizarBobina.Raise(this);
            }

            OnRetornoReceived.Raise(this, retorno);
            return retorno.Sucesso;
        }

        private RetornoEventArgs VendaNFCe()
        {
            var nfce = VendaAtual.ToNFCe();
            NFe.LimparLista();
            NFe.CarregarNota(nfce);

            var envioRet = NFe.Enviar(1, true, true);
            var ret = new RetornoEventArgs
            {
                Sucesso = false,
                Retorno = envioRet.Resposta
            };

            if (envioRet.Envio.CStat != 100) return ret;

            ret.Sucesso = true;
            Configuracao.Instance.DFe.NumeroAtual += 1;
            Configuracao.Instance.Save();
            return ret;
        }

        private RetornoEventArgs VendaSAT()
        {
            SAT.Inicializar();

            try
            {
                var cfe = VendaAtual.ToCFeIni();

                var envioRet = SAT.CriarEnviarCFe(cfe);
                var ret = new RetornoEventArgs
                {
                    Sucesso = false,
                    Retorno = envioRet.Resposta
                };

                if (envioRet.CodigoDeRetorno != 6000)
                    return ret;

                SAT.ImprimirExtratoVenda(envioRet.Arquivo);
                ret.Sucesso = true;
                return ret;
            }
            finally
            {
                SAT.DesInicializar();
            }
        }

        #endregion Venda

        #region Methods

        public void Imprimir(params string[] dados)
        {
            try
            {
                PosPrinter.Ativar();
                PosPrinter.Imprimir(string.Join(Environment.NewLine, dados));
            }
            finally
            {
                PosPrinter.Desativar();
            }
        }

        public void Inicializar()
        {
            MensagemCaixa = "Caixa Livre";
        }

        /// <inheritdoc />
        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        public void Dispose()
        {
            // Dispose all managed and unmanaged resources.
            Dispose(true);

            // Take this object off the finalization queue and prevent finalization code for this
            // object from executing a second time.
            GC.SuppressFinalize(this);
        }

        #endregion Methods

        #region Protected Methods

        /// <summary>
        /// Disposes the managed resources implementing <see cref="IDisposable"/>.
        /// </summary>
        protected virtual void DisposeManaged()
        {
        }

        /// <summary>
        /// Disposes the unmanaged resources implementing <see cref="IDisposable"/>.
        /// </summary>
        protected virtual void DisposeUnmanaged()
        {
            NFe.Dispose();
            SAT.Dispose();
            PosPrinter.Dispose();
            Bal.Dispose();
        }

        #endregion Protected Methods

        #region Private Methods

        /// <summary>
        /// Releases unmanaged and - optionally - managed resources.
        /// </summary>
        /// <param name="disposing"><c>true</c> to release both managed and unmanaged resources;
        /// <c>false</c> to release only unmanaged resources, called from the finalizer only.</param>
        private void Dispose(bool disposing)
        {
            // Check to see if Dispose has already been called.
            if (IsDisposed) return;

            // If disposing managed and unmanaged resources.
            if (disposing)
            {
                DisposeManaged();
            }

            DisposeUnmanaged();

            IsDisposed = true;
        }

        #endregion Private Methods
    }
}