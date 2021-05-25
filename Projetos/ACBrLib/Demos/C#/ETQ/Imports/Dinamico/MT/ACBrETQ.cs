using ACBrLib.Core;
using System;
using System.Text;
using ACBrLib.Core.ETQ;

namespace ACBrLib.ETQ
{
    public sealed partial class ACBrETQ : ACBrLibHandle
    {
        #region Constructors

        public ACBrETQ(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrETQ64.dll" : "ACBrETQ32.dll")
        {
            var inicializar = GetMethod<ETQ_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<ETQ_Nome>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public string Versao
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<ETQ_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<ETQ_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<ETQ_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<ETQ_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public override void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<ETQ_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<ETQ_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<ETQ_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void Ativar()
        {
            var method = GetMethod<ETQ_Ativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<ETQ_Desativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void IniciarEtiqueta()
        {
            var method = GetMethod<ETQ_IniciarEtiqueta>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void FinalizarEtiqueta(int aCopias = 1, int aAvancoEtq = 0)
        {
            var method = GetMethod<ETQ_FinalizarEtiqueta>();
            var ret = ExecuteMethod(() => method(libHandle, aCopias, aAvancoEtq));

            CheckResult(ret);
        }

        public void CarregarImagem(string eArquivoImagem, string eNomeImagem, bool flipped = true)
        {
            var method = GetMethod<ETQ_CarregarImagem>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoImagem), ToUTF8(eNomeImagem), flipped));

            CheckResult(ret);
        }

        public void Imprimir(int aCopias = 1, int aAvancoEtq = 0)
        {
            var method = GetMethod<ETQ_Imprimir>();
            var ret = ExecuteMethod(() => method(libHandle, aCopias, aAvancoEtq));

            CheckResult(ret);
        }

        public void ImprimirTexto(ETQOrientacao orientacao, int fonte, int multiplicadorH, int multiplicadorV,
                            int vertical, int horizontal, string eTexto, int subFonte = 0, bool imprimirReverso = false)
        {
            var method = GetMethod<ETQ_ImprimirTexto>();
            var ret = ExecuteMethod(() => method(libHandle, (int)orientacao, fonte, multiplicadorH, multiplicadorV,
                vertical, horizontal, ToUTF8(eTexto), subFonte, imprimirReverso));

            CheckResult(ret);
        }

        public void ImprimirTexto(ETQOrientacao orientacao, string fonte, int multiplicadorH, int multiplicadorV,
                            int vertical, int horizontal, string eTexto, int subFonte = 0, bool imprimirReverso = false)
        {
            var method = GetMethod<ETQ_ImprimirTextoStr>();
            var ret = ExecuteMethod(() => method(libHandle, (int)orientacao, ToUTF8(fonte), multiplicadorH, multiplicadorV,
                vertical, horizontal, ToUTF8(eTexto), subFonte, imprimirReverso));

            CheckResult(ret);
        }

        public void ImprimirBarras(ETQOrientacao orientacao, TipoCodBarra tipoBarras, int larguraBarraLarga, int larguraBarraFina,
                            int vertical, int horizontal, string eTexto, int alturaCodBarras = 0,
                            ETQBarraExibeCodigo exibeCodigo = ETQBarraExibeCodigo.becPadrao)
        {
            var method = GetMethod<ETQ_ImprimirBarras>();
            var ret = ExecuteMethod(() => method(libHandle, (int)orientacao, (int)tipoBarras, larguraBarraLarga, larguraBarraFina,
                vertical, horizontal, ToUTF8(eTexto), alturaCodBarras, (int)exibeCodigo));

            CheckResult(ret);
        }

        public void ImprimirLinha(int vertical, int horizontal, int largura, int altura)
        {
            var method = GetMethod<ETQ_ImprimirLinha>();
            var ret = ExecuteMethod(() => method(libHandle, vertical, horizontal, largura, altura));

            CheckResult(ret);
        }

        public void ImprimirCaixa(int vertical, int horizontal, int largura, int altura, int espessuraVertical,
                            int espessuraHorizontal)
        {
            var method = GetMethod<ETQ_ImprimirCaixa>();
            var ret = ExecuteMethod(() => method(libHandle, vertical, horizontal, largura, altura, espessuraVertical, espessuraHorizontal));

            CheckResult(ret);
        }

        public void ImprimirImagem(int multiplicadorImagem, int vertical, int horizontal, string eNomeImagem)
        {
            var method = GetMethod<ETQ_ImprimirImagem>();
            var ret = ExecuteMethod(() => method(libHandle, multiplicadorImagem, vertical, horizontal, ToUTF8(eNomeImagem)));

            CheckResult(ret);
        }

        public void ImprimirQRCode(int vertical, int horizontal, string texto, int larguraModulo, int errorLevel, int tipo)
        {
            var method = GetMethod<ETQ_ImprimirQRCode>();
            var ret = ExecuteMethod(() => method(libHandle, vertical, horizontal, ToUTF8(texto), larguraModulo, errorLevel, tipo));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<ETQ_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override void InitializeMethods()
        {
            AddMethod<ETQ_Inicializar>("ETQ_Inicializar");
            AddMethod<ETQ_Finalizar>("ETQ_Finalizar");
            AddMethod<ETQ_Nome>("ETQ_Nome");
            AddMethod<ETQ_Versao>("ETQ_Versao");
            AddMethod<ETQ_UltimoRetorno>("ETQ_UltimoRetorno");
            AddMethod<ETQ_ConfigImportar>("ETQ_ConfigImportar");
            AddMethod<ETQ_ConfigExportar>("ETQ_ConfigExportar");
            AddMethod<ETQ_ConfigLer>("ETQ_ConfigLer");
            AddMethod<ETQ_ConfigGravar>("ETQ_ConfigGravar");
            AddMethod<ETQ_ConfigLerValor>("ETQ_ConfigLerValor");
            AddMethod<ETQ_ConfigGravarValor>("ETQ_ConfigGravarValor");
            AddMethod<ETQ_Ativar>("ETQ_Ativar");
            AddMethod<ETQ_Desativar>("ETQ_Desativar");
            AddMethod<ETQ_IniciarEtiqueta>("ETQ_IniciarEtiqueta");
            AddMethod<ETQ_FinalizarEtiqueta>("ETQ_FinalizarEtiqueta");
            AddMethod<ETQ_CarregarImagem>("ETQ_CarregarImagem");
            AddMethod<ETQ_Imprimir>("ETQ_Imprimir");
            AddMethod<ETQ_ImprimirTexto>("ETQ_ImprimirTexto");
            AddMethod<ETQ_ImprimirTextoStr>("ETQ_ImprimirTextoStr");
            AddMethod<ETQ_ImprimirBarras>("ETQ_ImprimirBarras");
            AddMethod<ETQ_ImprimirLinha>("ETQ_ImprimirLinha");
            AddMethod<ETQ_ImprimirCaixa>("ETQ_ImprimirCaixa");
            AddMethod<ETQ_ImprimirImagem>("ETQ_ImprimirImagem");
            AddMethod<ETQ_ImprimirQRCode>("ETQ_ImprimirQRCode");
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<ETQ_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Methods
    }
}