using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.PosPrinter;

namespace ACBrLibPosPrinter
{
    /// <inheritdoc />
    public sealed class ACBrPosPrinter : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Inicializar(string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Finalizar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Nome(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Versao(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ConfigLer(string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ConfigGravar(string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ConfigGravarValor(string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Ativar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Desativar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Imprimir(string aString, bool pulaLinha, bool decodificarTags, bool codificarPagina, int copias);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ImprimirLinha(string aString);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ImprimirCmd(string aString);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_ImprimirTags();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_TxRx(string aString, byte bytesToRead, int aTimeOut, bool waitForTerminator, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Zerar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_InicializarPos();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_Reset();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_PularLinhas(int numLinhas);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_CortarPapel(bool parcial);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_AbrirGaveta();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_LerInfoImpressora(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_LerStatusImpressora(int tentativas, ref int status);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int POS_RetornarTags(StringBuilder buffer, ref int bufferSize, bool incluiAjuda);
        }

        #endregion InnerTypes

        #region Fields

        private const int BUFFER_LEN = 256;

        #endregion Fields

        #region Constructors

        public ACBrPosPrinter(string eArqConfig = "", string eChaveCrypt = "") : base(Environment.Is64BitProcess ? "ACBrPosPrinter64.dll" : "ACBrPosPrinter32.dll")
        {
            var inicializar = GetMethod<Delegates.POS_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

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

                var method = GetMethod<Delegates.POS_Nome>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

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

                var method = GetMethod<Delegates.POS_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Configurações

        #region Ini

        public void ConfigGravar(string eArqConfig = "ACBrLib.ini")
        {
            var gravarIni = GetMethod<Delegates.POS_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "ACBrLib.ini")
        {
            var lerIni = GetMethod<Delegates.POS_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.POS_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.POS_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        #region Ativar

        public void Ativar()
        {
            var method = GetMethod<Delegates.POS_Ativar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<Delegates.POS_Desativar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        #endregion Ativar

        #region Diversos

        public void Zerar()
        {
            var method = GetMethod<Delegates.POS_Zerar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Inicializar()
        {
            var method = GetMethod<Delegates.POS_InicializarPos>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Reset()
        {
            var method = GetMethod<Delegates.POS_Reset>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void PularLinhas(int numLinhas = 0)
        {
            var method = GetMethod<Delegates.POS_PularLinhas>();
            var ret = ExecuteMethod(() => method(numLinhas));

            CheckResult(ret);
        }

        public void CortarPapel(bool parcial = false)
        {
            var method = GetMethod<Delegates.POS_CortarPapel>();
            var ret = ExecuteMethod(() => method(parcial));

            CheckResult(ret);
        }

        public void AbrirGaveta()
        {
            var method = GetMethod<Delegates.POS_AbrirGaveta>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string LerInfoImpressora()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.POS_LerInfoImpressora>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public ACBrPosTipoStatus LerStatusImpressora(int tentativas = 1)
        {
            var status = 0;
            var method = GetMethod<Delegates.POS_LerStatusImpressora>();
            var ret = ExecuteMethod(() => method(tentativas, ref status));

            CheckResult(ret);

            return (ACBrPosTipoStatus)status;
        }

        public string[] RetornarTags(bool incluiAjuda = true)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.POS_RetornarTags>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen, incluiAjuda));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen).Split('|');
        }

        #endregion Diversos

        #region Imprimir

        public void Imprimir(string aString = "", bool pulaLinha = false, bool decodificarTags = true, bool codificarPagina = true, int copias = 1)
        {
            var method = GetMethod<Delegates.POS_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString), pulaLinha, decodificarTags, codificarPagina, copias));

            CheckResult(ret);
        }

        public void ImprimirLinha(string aString)
        {
            var method = GetMethod<Delegates.POS_ImprimirLinha>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString)));

            CheckResult(ret);
        }

        public void ImprimirCmd(string aString)
        {
            var method = GetMethod<Delegates.POS_ImprimirCmd>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString)));

            CheckResult(ret);
        }

        public void ImprimirTags()
        {
            var method = GetMethod<Delegates.POS_ImprimirTags>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string TxRx(string aString, byte bytesToRead = 1, int aTimeOut = 500, bool waitForTerminator = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.POS_TxRx>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString), bytesToRead, aTimeOut, waitForTerminator, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Imprimir

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.POS_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.POS_Inicializar>("POS_Inicializar");
            AddMethod<Delegates.POS_Finalizar>("POS_Finalizar");
            AddMethod<Delegates.POS_Nome>("POS_Nome");
            AddMethod<Delegates.POS_Versao>("POS_Versao");
            AddMethod<Delegates.POS_UltimoRetorno>("POS_UltimoRetorno");
            AddMethod<Delegates.POS_ConfigLer>("POS_ConfigLer");
            AddMethod<Delegates.POS_ConfigGravar>("POS_ConfigGravar");
            AddMethod<Delegates.POS_ConfigLerValor>("POS_ConfigLerValor");
            AddMethod<Delegates.POS_ConfigGravarValor>("POS_ConfigGravarValor");
            AddMethod<Delegates.POS_Ativar>("POS_Ativar");
            AddMethod<Delegates.POS_Desativar>("POS_Desativar");
            AddMethod<Delegates.POS_Imprimir>("POS_Imprimir");
            AddMethod<Delegates.POS_ImprimirLinha>("POS_ImprimirLinha");
            AddMethod<Delegates.POS_ImprimirCmd>("POS_ImprimirCmd");
            AddMethod<Delegates.POS_ImprimirTags>("POS_ImprimirTags");
            AddMethod<Delegates.POS_TxRx>("POS_TxRx");
            AddMethod<Delegates.POS_Zerar>("POS_Zerar");
            AddMethod<Delegates.POS_InicializarPos>("POS_InicializarPos");
            AddMethod<Delegates.POS_Reset>("POS_Reset");
            AddMethod<Delegates.POS_PularLinhas>("POS_PularLinhas");
            AddMethod<Delegates.POS_CortarPapel>("POS_CortarPapel");
            AddMethod<Delegates.POS_AbrirGaveta>("POS_AbrirGaveta");
            AddMethod<Delegates.POS_LerInfoImpressora>("POS_LerInfoImpressora");
            AddMethod<Delegates.POS_LerStatusImpressora>("POS_LerStatusImpressora");
            AddMethod<Delegates.POS_RetornarTags>("POS_RetornarTags");
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.POS_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Configurações
    }
}