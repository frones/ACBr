using ACBrLib.Core;
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core.ETQ;

namespace ACBrLib.ETQ
{
    public class ACBrETQ : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Finalizar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ConfigLer(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ConfigGravar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Ativar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Desativar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_IniciarEtiqueta(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_FinalizarEtiqueta(IntPtr handle, int aCopias, int aAvancoEtq);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_CarregarImagem(IntPtr handle, string eArquivoImagem, string eNomeImagem, bool flipped);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_Imprimir(IntPtr handle, int aCopias, int aAvancoEtq);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirTexto(IntPtr handle, int orientacao, int fonte, int multiplicadorH, int multiplicadorV,
                            int vertical, int horizontal, string eTexto, int subFonte, bool imprimirReverso);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirTextoStr(IntPtr handle, int orientacao, string fonte, int multiplicadorH, int multiplicadorV,
                            int vertical, int horizontal, string eTexto, int subFonte, bool imprimirReverso);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirBarras(IntPtr handle, int orientacao, int tipoBarras, int larguraBarraLarga, int larguraBarraFina,
                            int vertical, int horizontal, string eTexto, int alturaCodBarras, int exibeCodigo);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirLinha(IntPtr handle, int vertical, int horizontal, int largura, int altura);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirCaixa(IntPtr handle, int vertical, int horizontal, int largura, int altura, int espessuraVertical,
                            int espessuraHorizontal);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirImagem(IntPtr handle, int multiplicadorImagem, int vertical, int horizontal, string eNomeImagem);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int ETQ_ImprimirQRCode(IntPtr handle, int vertical, int horizontal, string texto, int larguraModulo, int errorLevel, int tipo);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrETQ(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrETQ64.dll" : "ACBrETQ32.dll")
        {
            var inicializar = GetMethod<Delegates.ETQ_Inicializar>();
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

                var method = GetMethod<Delegates.ETQ_Nome>();
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

                var method = GetMethod<Delegates.ETQ_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<Delegates.ETQ_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Delegates.ETQ_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.ETQ_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.ETQ_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void Ativar()
        {
            var method = GetMethod<Delegates.ETQ_Ativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<Delegates.ETQ_Desativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void IniciarEtiqueta()
        {
            var method = GetMethod<Delegates.ETQ_IniciarEtiqueta>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void FinalizarEtiqueta(int aCopias = 1, int aAvancoEtq = 0)
        {
            var method = GetMethod<Delegates.ETQ_FinalizarEtiqueta>();
            var ret = ExecuteMethod(() => method(libHandle, aCopias, aAvancoEtq));

            CheckResult(ret);
        }

        public void CarregarImagem(string eArquivoImagem, string eNomeImagem, bool flipped = true)
        {
            var method = GetMethod<Delegates.ETQ_CarregarImagem>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoImagem), ToUTF8(eNomeImagem), flipped));

            CheckResult(ret);
        }

        public void Imprimir(int aCopias = 1, int aAvancoEtq = 0)
        {
            var method = GetMethod<Delegates.ETQ_Imprimir>();
            var ret = ExecuteMethod(() => method(libHandle, aCopias, aAvancoEtq));

            CheckResult(ret);
        }

        public void ImprimirTexto(ETQOrientacao orientacao, int fonte, int multiplicadorH, int multiplicadorV,
                            int vertical, int horizontal, string eTexto, int subFonte = 0, bool imprimirReverso = false)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirTexto>();
            var ret = ExecuteMethod(() => method(libHandle, (int)orientacao, fonte, multiplicadorH, multiplicadorV,
                vertical, horizontal, ToUTF8(eTexto), subFonte, imprimirReverso));

            CheckResult(ret);
        }

        public void ImprimirTexto(ETQOrientacao orientacao, string fonte, int multiplicadorH, int multiplicadorV,
                            int vertical, int horizontal, string eTexto, int subFonte = 0, bool imprimirReverso = false)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirTextoStr>();
            var ret = ExecuteMethod(() => method(libHandle, (int)orientacao, ToUTF8(fonte), multiplicadorH, multiplicadorV,
                vertical, horizontal, ToUTF8(eTexto), subFonte, imprimirReverso));

            CheckResult(ret);
        }

        public void ImprimirBarras(ETQOrientacao orientacao, TipoCodBarra tipoBarras, int larguraBarraLarga, int larguraBarraFina,
                            int vertical, int horizontal, string eTexto, int alturaCodBarras = 0,
                            ETQBarraExibeCodigo exibeCodigo = ETQBarraExibeCodigo.becPadrao)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirBarras>();
            var ret = ExecuteMethod(() => method(libHandle, (int)orientacao, (int)tipoBarras, larguraBarraLarga, larguraBarraFina,
                vertical, horizontal, ToUTF8(eTexto), alturaCodBarras, (int)exibeCodigo));

            CheckResult(ret);
        }

        public void ImprimirLinha(int vertical, int horizontal, int largura, int altura)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirLinha>();
            var ret = ExecuteMethod(() => method(libHandle, vertical, horizontal, largura, altura));

            CheckResult(ret);
        }

        public void ImprimirCaixa(int vertical, int horizontal, int largura, int altura, int espessuraVertical,
                            int espessuraHorizontal)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirCaixa>();
            var ret = ExecuteMethod(() => method(libHandle, vertical, horizontal, largura, altura, espessuraVertical, espessuraHorizontal));

            CheckResult(ret);
        }

        public void ImprimirImagem(int multiplicadorImagem, int vertical, int horizontal, string eNomeImagem)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirImagem>();
            var ret = ExecuteMethod(() => method(libHandle, multiplicadorImagem, vertical, horizontal, ToUTF8(eNomeImagem)));

            CheckResult(ret);
        }

        public void ImprimirQRCode(int vertical, int horizontal, string texto, int larguraModulo, int errorLevel, int tipo)
        {
            var method = GetMethod<Delegates.ETQ_ImprimirQRCode>();
            var ret = ExecuteMethod(() => method(libHandle, vertical, horizontal, ToUTF8(texto), larguraModulo, errorLevel, tipo));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.ETQ_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.ETQ_Inicializar>("ETQ_Inicializar");
            AddMethod<Delegates.ETQ_Finalizar>("ETQ_Finalizar");
            AddMethod<Delegates.ETQ_Nome>("ETQ_Nome");
            AddMethod<Delegates.ETQ_Versao>("ETQ_Versao");
            AddMethod<Delegates.ETQ_UltimoRetorno>("ETQ_UltimoRetorno");
            AddMethod<Delegates.ETQ_ConfigLer>("ETQ_ConfigLer");
            AddMethod<Delegates.ETQ_ConfigGravar>("ETQ_ConfigGravar");
            AddMethod<Delegates.ETQ_ConfigLerValor>("ETQ_ConfigLerValor");
            AddMethod<Delegates.ETQ_ConfigGravarValor>("ETQ_ConfigGravarValor");
            AddMethod<Delegates.ETQ_Ativar>("ETQ_Ativar");
            AddMethod<Delegates.ETQ_Desativar>("ETQ_Desativar");
            AddMethod<Delegates.ETQ_IniciarEtiqueta>("ETQ_IniciarEtiqueta");
            AddMethod<Delegates.ETQ_FinalizarEtiqueta>("ETQ_FinalizarEtiqueta");
            AddMethod<Delegates.ETQ_CarregarImagem>("ETQ_CarregarImagem");
            AddMethod<Delegates.ETQ_Imprimir>("ETQ_Imprimir");
            AddMethod<Delegates.ETQ_ImprimirTexto>("ETQ_ImprimirTexto");
            AddMethod<Delegates.ETQ_ImprimirTextoStr>("ETQ_ImprimirTextoStr");
            AddMethod<Delegates.ETQ_ImprimirBarras>("ETQ_ImprimirBarras");
            AddMethod<Delegates.ETQ_ImprimirLinha>("ETQ_ImprimirLinha");
            AddMethod<Delegates.ETQ_ImprimirCaixa>("ETQ_ImprimirCaixa");
            AddMethod<Delegates.ETQ_ImprimirImagem>("ETQ_ImprimirImagem");
            AddMethod<Delegates.ETQ_ImprimirQRCode>("ETQ_ImprimirQRCode");
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.ETQ_UltimoRetorno>();

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