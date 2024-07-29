using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;

namespace ACBrLib.PIXCD
{
    /// <inheritdoc />
    public sealed partial class ACBrPIXCD : ACBrLibHandle
    {
        #region Constructors

        public ACBrPIXCD(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrPIXCD64.dll" : "libacbrpixcd64.so",
                                                                                      IsWindows ? "ACBrPIXCD32.dll" : "libacbrpixcd32.so")
        {
            var inicializar = GetMethod<PIXCD_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new ACBrPIXCDConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<PIXCD_Nome>();
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

                var method = GetMethod<PIXCD_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public ACBrPIXCDConfig Config { get; }

        #endregion Properties

        #region Metodos

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<PIXCD_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var lerIni = GetMethod<PIXCD_ConfigImportar>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<PIXCD_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<PIXCD_ConfigLerValor>();

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

            var method = GetMethod<PIXCD_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        #region Diversos
        public string OpenSSLInfo()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_OpenSSLInfo>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string IncluirConta(string aInfIncluirConta)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_IncluirConta>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aInfIncluirConta), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarConta(string aAccountId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarConta>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string InativarConta(string aAccountId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_InativarConta>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string IncluirChavePix(string aAccountId, string aExternalID)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_IncluirChavePix>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), ToUTF8(aExternalID), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarChavePix(string aAccountId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarChavePix>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ExcluirChavePix(string aAccountId, string aChavePIX)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ExcluirChavePix>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), ToUTF8(aChavePIX), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarQRCode(string aInfQRCode)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_GerarQRCode>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aInfQRCode), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarTransacao(string aAccountId, string aTransactionID)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarTransacao>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), ToUTF8(aTransactionID), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarSaldoEC(string aAccountId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarSaldoEC>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarExtratoEC(string aAccountId, DateTime aInicio, DateTime aFim)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarExtratoEC>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), aInicio, aFim, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarMotivosDevolucao()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarMotivosDevolucao>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string SolicitarDevolucao(string aInfSolicitarDevolucao, string aAccountId, string aTransactionID)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_SolicitarDevolucao>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aInfSolicitarDevolucao), ToUTF8(aAccountId), ToUTF8(aTransactionID), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarAliasRetirada(string aAccountId, string aAlias)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_ConsultarAliasRetirada>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aAccountId), ToUTF8(aAlias), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string SolicitarRetirada(string aInfSolicitarRetirada, string aAccountId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_Matera_SolicitarRetirada>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aInfSolicitarRetirada), ToUTF8(aAccountId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }


        #endregion Diversos

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<PIXCD_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<PIXCD_UltimoRetorno>();

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

        #endregion Metodos
    }
}