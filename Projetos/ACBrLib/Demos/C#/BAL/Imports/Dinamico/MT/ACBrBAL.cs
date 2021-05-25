using System.Text;
using ACBrLib.Core;

namespace ACBrLib.BAL
{
    public sealed partial class ACBrBAL : ACBrLibHandle
    {
        #region Constructors

        public ACBrBAL(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrBAL64.dll" : "libacbrbal64.so",
                                                                               IsWindows ? "ACBrBAL32.dll" : "libacbrbal32.so")
        {
            var inicializar = GetMethod<BAL_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new BALConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<BAL_Nome>();
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

                var method = GetMethod<BAL_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public BALConfig Config { get; }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<BAL_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<BAL_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<BAL_ConfigLerValor>();

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

            var method = GetMethod<BAL_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<BAL_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<BAL_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void Ativar()
        {
            var method = GetMethod<BAL_Ativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<BAL_Desativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public decimal LePeso(int MillisecTimeOut = 1000)
        {
            var peso = 0D;
            var method = GetMethod<BAL_LePeso>();
            var ret = ExecuteMethod(() => method(libHandle, MillisecTimeOut, ref peso));

            CheckResult(ret);

            return (decimal)peso;
        }

        public void SolicitarPeso()
        {
            var method = GetMethod<BAL_SolicitarPeso>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public decimal UltimoPesoLido()
        {
            var peso = 0D;
            var method = GetMethod<BAL_UltimoPesoLido>();
            var ret = ExecuteMethod(() => method(libHandle, ref peso));

            CheckResult(ret);

            return (decimal)peso;
        }

        public decimal InterpretarRespostaPeso(string resposta)
        {
            var peso = 0D;
            var method = GetMethod<BAL_InterpretarRespostaPeso>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(resposta), ref peso));

            CheckResult(ret);

            return (decimal)peso;
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<BAL_Finalizar>();
            var ret = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<BAL_UltimoRetorno>();

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