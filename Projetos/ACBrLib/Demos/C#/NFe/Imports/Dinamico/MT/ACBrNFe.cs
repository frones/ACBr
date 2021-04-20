using System;
using System.Linq;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    public sealed partial class ACBrNFe : ACBrLibHandle
    {
        #region Constructors

        public ACBrNFe(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrNFe64.dll" : "libacbrnfe64.so",
                                                                               IsWindows ? "ACBrNFe32.dll" : "libacbrnfe32.so")
        {
            var inicializar = GetMethod<NFE_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<NFE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<NFE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<NFE_ConfigLerValor>();

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

            var method = GetMethod<NFE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var importarConfig = GetMethod<NFE_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<NFE_CarregarXML>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<NFE_CarregarINI>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ObterXml>();
            var ret = ExecuteMethod(() => method(libHandle, aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<NFE_GravarXml>();
            var ret = ExecuteMethod(() => method(libHandle, aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public string ObterIni(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ObterIni>();
            var ret = ExecuteMethod(() => method(libHandle, aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarIni(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<NFE_GravarIni>();
            var ret = ExecuteMethod(() => method(libHandle, aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarEventoXML(string eArquivoOuXml)
        {
            var method = GetMethod<NFE_CarregarEventoXML>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarEventoINI(string eArquivoOuIni)
        {
            var method = GetMethod<NFE_CarregarEventoINI>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<NFE_LimparLista>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void LimparListaEventos()
        {
            var method = GetMethod<NFE_LimparListaEventos>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<NFE_Assinar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<NFE_Validar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public string ValidarRegrasdeNegocios()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ValidarRegrasdeNegocios>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_VerificarAssinatura>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, DateTime aEmissao, string acpfcnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_GerarChave>();
            var ret = ExecuteMethod(() => method(libHandle, aCodigoUf, aCodigoNumerico, aModelo, aSerie, aNumero,
                                                 aTpEmi, aEmissao.Date.ToString("dd/MM/yyyy"), ToUTF8(acpfcnpj),
                                                 buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ObterCertificados>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string GetPath(TipoPathNFe tipo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_GetPath>();
            var ret = ExecuteMethod(() => method(libHandle, (int)tipo, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPathEvento(string evento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_GetPathEvento>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(evento), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string StatusServico()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_StatusServico>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Consultar(string eChaveOuNFe, bool AExtrairEventos = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_Consultar>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eChaveOuNFe), AExtrairEventos, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultaCadastro(string cUF, string nDocumento, bool nIE)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ConsultaCadastro>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(cUF), ToUTF8(nDocumento), nIE, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_Inutilizar>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(acnpj), ToUTF8(aJustificativa), ano, modelo, serie, numeroInicial, numeroFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Enviar(int aLote, bool imprimir = false, bool sincrono = false, bool zipado = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_Enviar>();
            var ret = ExecuteMethod(() => method(libHandle, aLote, imprimir, sincrono, zipado, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarRecibo(string aRecibo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_ConsultarRecibo>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(aRecibo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_Cancelar>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eChave), ToUTF8(eJustificativa), ToUTF8(eCNPJ), aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string EnviarEvento(int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_EnviarEvento>();
            var ret = ExecuteMethod(() => method(libHandle, aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_DistribuicaoDFePorUltNSU>();
            var ret = ExecuteMethod(() => method(libHandle, acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_DistribuicaoDFePorNSU>();
            var ret = ExecuteMethod(() => method(libHandle, acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echNFe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFE_DistribuicaoDFePorChave>();
            var ret = ExecuteMethod(() => method(libHandle, acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(echNFe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<NFE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ePara), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                                                 ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<NFE_EnviarEmailEvento>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ePara), ToUTF8(eChaveEvento), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, string cProtocolo = "", bool? bMostrarPreview = null, bool? cMarcaDagua = null,
            bool? bViaConsumidor = null, bool? bSimplificado = null)
        {
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;
            var marcaDagua = cMarcaDagua.HasValue ? $"{Convert.ToInt32(cMarcaDagua.Value)}" : string.Empty;
            var viaConsumidor = bViaConsumidor.HasValue ? $"{Convert.ToInt32(bViaConsumidor.Value)}" : string.Empty;
            var simplificado = bSimplificado.HasValue ? $"{Convert.ToInt32(bSimplificado.Value)}" : string.Empty;

            var method = GetMethod<NFE_Imprimir>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(cImpressora), nNumCopias, ToUTF8(cProtocolo), ToUTF8(mostrarPreview),
                ToUTF8(marcaDagua), ToUTF8(viaConsumidor), ToUTF8(simplificado)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<NFE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<NFE_ImprimirEvento>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<NFE_ImprimirEventoPDF>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacao(string eArquivoXml)
        {
            var method = GetMethod<NFE_ImprimirInutilizacao>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacaoPDF(string eArquivoXml)
        {
            var method = GetMethod<NFE_ImprimirInutilizacaoPDF>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<NFE_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override void InitializeMethods()
        {
            AddMethod<NFE_Inicializar>("NFE_Inicializar");
            AddMethod<NFE_Finalizar>("NFE_Finalizar");
            AddMethod<NFE_Nome>("NFE_Nome");
            AddMethod<NFE_Versao>("NFE_Versao");
            AddMethod<NFE_UltimoRetorno>("NFE_UltimoRetorno");
            AddMethod<NFE_ConfigImportar>("NFE_ConfigImportar");
            AddMethod<NFE_ConfigExportar>("NFE_ConfigExportar");
            AddMethod<NFE_ConfigLer>("NFE_ConfigLer");
            AddMethod<NFE_ConfigGravar>("NFE_ConfigGravar");
            AddMethod<NFE_ConfigLerValor>("NFE_ConfigLerValor");
            AddMethod<NFE_ConfigGravarValor>("NFE_ConfigGravarValor");
            AddMethod<NFE_CarregarXML>("NFE_CarregarXML");
            AddMethod<NFE_CarregarINI>("NFE_CarregarINI");
            AddMethod<NFE_ObterXml>("NFE_ObterXml");
            AddMethod<NFE_GravarXml>("NFE_GravarXml");
            AddMethod<NFE_ObterIni>("NFE_ObterIni");
            AddMethod<NFE_GravarIni>("NFE_GravarIni");
            AddMethod<NFE_CarregarEventoXML>("NFE_CarregarEventoXML");
            AddMethod<NFE_CarregarEventoINI>("NFE_CarregarEventoINI");
            AddMethod<NFE_LimparLista>("NFE_LimparLista");
            AddMethod<NFE_LimparListaEventos>("NFE_LimparListaEventos");
            AddMethod<NFE_Assinar>("NFE_Assinar");
            AddMethod<NFE_Validar>("NFE_Validar");
            AddMethod<NFE_ValidarRegrasdeNegocios>("NFE_ValidarRegrasdeNegocios");
            AddMethod<NFE_VerificarAssinatura>("NFE_VerificarAssinatura");
            AddMethod<NFE_GerarChave>("NFE_GerarChave");
            AddMethod<NFE_ObterCertificados>("NFE_ObterCertificados");
            AddMethod<NFE_GetPath>("NFE_GetPath");
            AddMethod<NFE_GetPathEvento>("NFE_GetPathEvento");
            AddMethod<NFE_StatusServico>("NFE_StatusServico");
            AddMethod<NFE_Consultar>("NFE_Consultar");
            AddMethod<NFE_ConsultaCadastro>("NFE_ConsultaCadastro");
            AddMethod<NFE_Inutilizar>("NFE_Inutilizar");
            AddMethod<NFE_Enviar>("NFE_Enviar");
            AddMethod<NFE_ConsultarRecibo>("NFE_ConsultarRecibo");
            AddMethod<NFE_Cancelar>("NFE_Cancelar");
            AddMethod<NFE_EnviarEvento>("NFE_EnviarEvento");
            AddMethod<NFE_DistribuicaoDFePorUltNSU>("NFE_DistribuicaoDFePorUltNSU");
            AddMethod<NFE_DistribuicaoDFePorNSU>("NFE_DistribuicaoDFePorNSU");
            AddMethod<NFE_DistribuicaoDFePorChave>("NFE_DistribuicaoDFePorChave");
            AddMethod<NFE_EnviarEmail>("NFE_EnviarEmail");
            AddMethod<NFE_EnviarEmailEvento>("NFE_EnviarEmailEvento");
            AddMethod<NFE_Imprimir>("NFE_Imprimir");
            AddMethod<NFE_ImprimirPDF>("NFE_ImprimirPDF");
            AddMethod<NFE_ImprimirEvento>("NFE_ImprimirEvento");
            AddMethod<NFE_ImprimirEventoPDF>("NFE_ImprimirEventoPDF");
            AddMethod<NFE_ImprimirInutilizacao>("NFE_ImprimirInutilizacao");
            AddMethod<NFE_ImprimirInutilizacaoPDF>("NFE_ImprimirInutilizacaoPDF");
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<NFE_UltimoRetorno>();

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