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

        public string GerarQRCodeEstatico(double AValor, string AinfoAdicional, string ATxID)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_GerarQRCodeEstatico>();
            var ret = ExecuteMethod(() => method(libHandle, AValor, ToUTF8(AinfoAdicional), ToUTF8(ATxID), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarPix(string Ae2eid)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_ConsultarPix>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(Ae2eid), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarPixRecebidos(DateTime ADataInicio, DateTime ADataFim, string ATxId, string ACpfCnpj, int PagAtual, int ItensPorPagina)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_ConsultarPixRecebidos>();
            var ret = ExecuteMethod(() => method(libHandle, ADataInicio, ADataFim, ToUTF8(ATxId), ToUTF8(ACpfCnpj), PagAtual, ItensPorPagina, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string SolicitarDevolucaoPix(string AInfDevolucao, string Ae2eid, string AidDevolucao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_SolicitarDevolucaoPix>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(AInfDevolucao), ToUTF8(Ae2eid), ToUTF8(AidDevolucao), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarDevolucaoPix(string Ae2eid, string AidDevolucao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_ConsultarDevolucaoPix>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(Ae2eid), ToUTF8(AidDevolucao), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CriarCobrancaImediata(string AInfCobSolicitada, string ATxId)
        {

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_CriarCobrancaImediata>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(AInfCobSolicitada), ToUTF8(ATxId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarCobrancaImediata(string ATxId, int ARevisao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_ConsultarCobrancaImediata>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ATxId), ARevisao, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string RevisarCobrancaImediata(string AInfCobRevisada, string ATxId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_RevisarCobrancaImediata>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(AInfCobRevisada), ToUTF8(ATxId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CancelarCobrancaImediata(string ATxId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_CancelarCobrancaImediata>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ATxId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CriarCobranca(string AInfCobVSolicitada, string ATxId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_CriarCobranca>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(AInfCobVSolicitada), ToUTF8(ATxId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarCobranca(string ATxId, int ARevisao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_ConsultarCobranca>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ATxId), ARevisao, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string RevisarCobranca(string AInfCobVRevisada, string ATxId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_RevisarCobranca>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(AInfCobVRevisada), ToUTF8(ATxId), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CancelarCobranca(string ATxId)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<PIXCD_CancelarCobranca>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ATxId), buffer, ref bufferLen));

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