const path = require("path");
const koffi = require("koffi");

const pathDllACBrLibNFSe = path.resolve(__dirname, "ACBrNFSe64.dll");
const eArqConfig = path.resolve(__dirname, "assets/acbr", "acbrlib.ini");
const pathCert = path.resolve(__dirname, "assets/cert", "certificado.pfx");
const pathSchemas = path.join(__dirname, "assets/acbr", "Schemas", "NFSe");
const pathIniSchemas = path.resolve(
  __dirname,
  "assets/acbr",
  "ACBrNFSeXServicos.ini"
);

const eChaveCrypt = "";

const acbrnfse = koffi.load(pathDllACBrLibNFSe);
const TAMANHO_BUFFER = 4096;
const lib = {
  NFSE_Inicializar: acbrnfse.func("NFSE_Inicializar", 'int', ['string', 'string']),
  NFSE_Finalizar: acbrnfse.func("NFSE_Finalizar", 'int', []),
  NFSE_Emitir: acbrnfse.func("NFSE_Emitir", 'int', ['string', 'int', 'bool', 'char*', 'int*']),
  NFSE_UltimoRetorno: acbrnfse.func("NFSE_UltimoRetorno", 'int', ['char*', 'int*']),
  NFSE_Nome: acbrnfse.func("NFSE_Nome", 'int', ['char*', 'int*']),
  NFSE_Versao: acbrnfse.func("NFSE_Versao", 'int', ['char*', 'int*']),
  NFSE_ConfigLer: acbrnfse.func("NFSE_ConfigLer", 'int', ['string']),
  NFSE_ConfigGravar: acbrnfse.func("NFSE_ConfigGravar", 'int', ['string']),
  NFSE_ConfigLerValor: acbrnfse.func("NFSE_ConfigLerValor", 'int', ['string', 'string', 'char*', 'int*']),
  NFSE_ConfigGravarValor: acbrnfse.func("NFSE_ConfigGravarValor", 'int', ['string', 'string', 'string']),
  NFSE_ConfigImportar: acbrnfse.func("NFSE_ConfigImportar", 'int', ['string']),
  NFSE_ConfigExportar: acbrnfse.func("NFSE_ConfigExportar", 'int', ['char*', 'int*']),
  NFSE_CarregarXML: acbrnfse.func("NFSE_CarregarXML", 'int', ['string']),
  NFSE_CarregarINI: acbrnfse.func("NFSE_CarregarINI", 'int', ['string']),
  NFSE_ObterXml: acbrnfse.func("NFSE_ObterXml", 'int', ['int', 'char*', 'int*']),
  NFSE_GravarXml: acbrnfse.func("NFSE_GravarXml", 'int', ['int', 'string', 'string']),
  NFSE_ObterIni: acbrnfse.func("NFSE_ObterIni", 'int', ['int', 'char*', 'int*']),
  NFSE_GravarIni: acbrnfse.func("NFSE_GravarIni", 'int', ['int', 'string', 'string']),
  NFSE_LimparLista: acbrnfse.func("NFSE_LimparLista", 'int', []),
  NFSE_ObterCertificados: acbrnfse.func("NFSE_ObterCertificados", 'int', ['char*', 'int*']),
  NFSE_Cancelar: acbrnfse.func("NFSE_Cancelar", 'int', ['string', 'string', 'string', 'int', 'char*', 'int*']),
  NFSE_EnviarEvento: acbrnfse.func("NFSE_EnviarEvento", 'int', ['int', 'char*', 'int*']),
  NFSE_EnviarEmail: acbrnfse.func("NFSE_EnviarEmail", 'int', ['string', 'string', 'bool', 'string', 'string', 'string', 'string']),
  NFSE_Imprimir: acbrnfse.func("NFSE_Imprimir", 'int', ['string', 'int', 'string', 'string', 'string', 'string', 'string']),
  NFSE_ImprimirPDF: acbrnfse.func("NFSE_ImprimirPDF", 'int', [])
};

function getNFe() {
  let inicio = 2;

  let aloc_sResposta = Buffer.alloc(TAMANHO_BUFFER);
  let aloc_esTamanho = koffi.alloc('int', 1);

  //configura tamanho do buffer
  koffi.encode(aloc_esTamanho, 'int', TAMANHO_BUFFER)

  console.log(`Iniciando >>>>>>>`);
  inicio = lib.NFSE_Inicializar(eArqConfig, eChaveCrypt);
  console.log(`iniciou >>>>>>> ${inicio}`);

  inicio = lib.NFSE_ConfigGravarValor("DFe", "ArquivoPFX", pathCert);
  console.log(`Set Configurações ArquivoPFX ${inicio}`);

  inicio = lib.NFSE_ConfigGravarValor("DFe", "Senha", '1234');
  console.log(`Set Configurações sENHA ${inicio}`);

  inicio = lib.NFSE_ConfigGravarValor("NFSe", "PathSchemas", pathSchemas);
  console.log(`Set Configurações PathSchemas ${inicio}`);

  inicio = lib.NFSE_ConfigGravarValor("NFSe", "IniServicos", pathIniSchemas);
  console.log(`Set Configurações IniServicos ${inicio}`);

  aloc_sResposta = Buffer.alloc(TAMANHO_BUFFER);
  aloc_esTamanho = koffi.alloc('int', 1);
  koffi.encode(aloc_esTamanho, 'int', TAMANHO_BUFFER)
  inicio = lib.NFSE_Emitir("55", 0, false, aloc_sResposta, aloc_esTamanho);

  console.log(`Mensagem: `, aloc_sResposta.toString());
  console.log(`NFSE_Emitir >>>>>>>> ${inicio}`);

  inicio = lib.NFSE_Finalizar();
  console.log(`finalizar >>>>>>>> ${inicio}`);
}

getNFe();
