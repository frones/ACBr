using System;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;

namespace ACBrLib.BAL
{
    public sealed class ACBrBAL : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_Finalizar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_ConfigImportar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_ConfigLer(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_ConfigGravar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_Ativar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_Desativar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_LePeso(IntPtr handle, int MillisecTimeOut, ref double peso);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_SolicitarPeso(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_UltimoPesoLido(IntPtr handle, ref double peso);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int BAL_InterpretarRespostaPeso(IntPtr handle, string resposta, ref double peso);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrBAL(string eArqConfig = "", string eChaveCrypt = "") :
            base("ACBrBAL64.dll", "ACBrBAL32.dll")
        {
            var inicializar = GetMethod<Delegates.BAL_Inicializar>();
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

                var method = GetMethod<Delegates.BAL_Nome>();
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

                var method = GetMethod<Delegates.BAL_Versao>();
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
            var gravarIni = GetMethod<Delegates.BAL_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Delegates.BAL_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.BAL_ConfigLerValor>();

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

            var method = GetMethod<Delegates.BAL_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<Delegates.BAL_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.BAL_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void Ativar()
        {
            var method = GetMethod<Delegates.BAL_Ativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<Delegates.BAL_Desativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public decimal LePeso(int MillisecTimeOut = 1000)
        {
            var peso = 0D;
            var method = GetMethod<Delegates.BAL_LePeso>();
            var ret = ExecuteMethod(() => method(libHandle, MillisecTimeOut, ref peso));

            CheckResult(ret);

            return (decimal)peso;
        }

        public void SolicitarPeso()
        {
            var method = GetMethod<Delegates.BAL_SolicitarPeso>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public decimal UltimoPesoLido()
        {
            var peso = 0D;
            var method = GetMethod<Delegates.BAL_UltimoPesoLido>();
            var ret = ExecuteMethod(() => method(libHandle, ref peso));

            CheckResult(ret);

            return (decimal)peso;
        }

        public decimal InterpretarRespostaPeso(string resposta)
        {
            var peso = 0D;
            var method = GetMethod<Delegates.BAL_InterpretarRespostaPeso>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(resposta), ref peso));

            CheckResult(ret);

            return (decimal)peso;
        }

        #region Private Methods

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.BAL_Inicializar>("BAL_Inicializar");
            AddMethod<Delegates.BAL_Finalizar>("BAL_Finalizar");
            AddMethod<Delegates.BAL_Nome>("BAL_Nome");
            AddMethod<Delegates.BAL_Versao>("BAL_Versao");
            AddMethod<Delegates.BAL_UltimoRetorno>("BAL_UltimoRetorno");
            AddMethod<Delegates.BAL_ConfigImportar>("BAL_ConfigImportar");
            AddMethod<Delegates.BAL_ConfigExportar>("BAL_ConfigExportar");
            AddMethod<Delegates.BAL_ConfigLer>("BAL_ConfigLer");
            AddMethod<Delegates.BAL_ConfigGravar>("BAL_ConfigGravar");
            AddMethod<Delegates.BAL_ConfigLerValor>("BAL_ConfigLerValor");
            AddMethod<Delegates.BAL_ConfigGravarValor>("BAL_ConfigGravarValor");
            AddMethod<Delegates.BAL_Ativar>("BAL_Ativar");
            AddMethod<Delegates.BAL_Desativar>("BAL_Desativar");
            AddMethod<Delegates.BAL_LePeso>("BAL_LePeso");
            AddMethod<Delegates.BAL_SolicitarPeso>("BAL_SolicitarPeso");
            AddMethod<Delegates.BAL_UltimoPesoLido>("BAL_UltimoPesoLido");
            AddMethod<Delegates.BAL_InterpretarRespostaPeso>("BAL_InterpretarRespostaPeso");
        }

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.BAL_Finalizar>();
            var ret = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.BAL_UltimoRetorno>();

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