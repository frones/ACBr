using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.eSocial;

namespace ACBrLib.eSocial
{
    /// <inheritdoc />
    public sealed partial class ACBreSocial : ACBrLibHandle
    {
        #region Constructors

        public ACBreSocial(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBreSocial64.dll" : "libacbresocial64.so",
                                                                                      IsWindows ? "ACBreSocial32.dll" : "libacbresocial32.so")
        {
            var inicializar = GetMethod<eSocial_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new ACBreSocialConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<eSocial_Nome>();
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

                var method = GetMethod<eSocial_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public ACBreSocialConfig Config { get; }

        #endregion Properties

        #region Metodos

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<eSocial_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var lerIni = GetMethod<eSocial_ConfigImportar>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<eSocial_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<eSocial_ConfigLerValor>();

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

            var method = GetMethod<eSocial_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        #region Diversos

        public void CriarEventoeSocial (string eArqIni)
        {
            var method = GetMethod<eSocial_CriarEventoeSocial>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArqIni)));

            CheckResult(ret);
        }

        public string EnviareSocial(int aGrupo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_EnviareSocial>();
            var ret = ExecuteMethod(() => method(libHandle, aGrupo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultareSocial(string eProtocolo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_ConsultareSocial>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eProtocolo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void CriarEnviareSocial(string eArqIni, int aGrupo)
        {
            var method = GetMethod<eSocial_CriarEnviareSocial>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArqIni), aGrupo));

            CheckResult(ret);
        }

        public void LimpareSocial()
        {
            var method = GetMethod<eSocial_LimpareSocial>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void CarregarXMLEventoeSocial(string eArquivoOuXML)
        {
            var method = GetMethod<eSocial_CarregarXMLEventoeSocial>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuXML)));

            CheckResult(ret);
        }

        public void SetIDEmpregador(string aIdEmpregador)
        {
            var method = GetMethod<eSocial_SetIDEmpregador>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aIdEmpregador)));

            CheckResult(ret);
        }

        public void SetIDTransmissor(string aIdTransmissor)
        {
            var method = GetMethod<eSocial_SetIDTransmissor>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aIdTransmissor)));

            CheckResult(ret);
        }

        public void TipoEmpregador(int aTipoEmpregador)
        {
            var method = GetMethod<eSocial_SetTipoEmpregador>();
            var ret = ExecuteMethod(() => method(libHandle, aTipoEmpregador));

            CheckResult(ret);
        }

        public void SetVersao(string sVersao)
        {
            var method = GetMethod<eSocial_SetVersaoDF>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(sVersao)));

            CheckResult(ret);
        }

        public string ConsultaIdentificadoresEventosEmpregador(string aIdEmpregador, int aTipoEvento, DateTime aPeriodoApuracao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_ConsultaIdentificadoresEventosEmpregador>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aIdEmpregador), aTipoEvento, aPeriodoApuracao, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultaIdentificadoresEventosTabela(string aIdEmpregador, int aTipoEvento, string aChave, DateTime aDataInicial, DateTime aDataFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_ConsultaIdentificadoresEventosTabela>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aIdEmpregador), aTipoEvento, aChave, aDataInicial, aDataFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultaIdentificadoresEventosTrabalhador(string aIdEmpregador, string aCPFTrabalhador, DateTime aDataInicial, DateTime aDataFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_ConsultaIdentificadoresEventosTrabalhador>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aIdEmpregador), aCPFTrabalhador, aDataInicial, aDataFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DownloadEventos(string aIdEmpregador, string aCPFTrabalhador, DateTime aDataInicial, DateTime aDataFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_DownloadEventos>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aIdEmpregador), aCPFTrabalhador, aDataInicial, aDataFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_ObterCertificados>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string OpenSSLInfo()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<eSocial_OpenSSLInfo>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Diversos

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<eSocial_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<eSocial_UltimoRetorno>();

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