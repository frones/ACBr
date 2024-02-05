const path = require("path");
const ffi = require("ffi-napi");
const ref = require("ref-napi");

const pathDllACBrLibNFSe = path.resolve(__dirname, "libacbrnfse64.so");
const eArqConfig = path.resolve(__dirname, "assets/acbr", "acbrlib.ini");
const pathCert = path.resolve(__dirname, "assets/cert", "certificado.pfx");
const pathSchemas = path.join(__dirname, "assets/acbr", "Schemas", "NFSe");
const pathIniSchemas = path.resolve(
  __dirname,
  "assets/acbr",
  "ACBrNFSeXServicos.ini"
);

var tint = ref.refType('int');
var tchar = ref.refType('char *');
const eChaveCrypt = "";

function getNFe() {
  let libm = ffi.Library(pathDllACBrLibNFSe, {
    NFSE_Inicializar: ["int", ["string", "string"]],

    NFSE_Finalizar: ["int", []],

    NFSE_Emitir: ["int", ["string", 'int', 'bool', tchar, tint]],

    NFSE_UltimoRetorno: ["int", ["string", "string"]],

    NFSE_Nome: ["int", ["string", "string"]],

    NFSE_Versao: ["int", ["string", "string"]],

    NFSE_ConfigLer: ["int", ["string"]],

    NFSE_ConfigGravar: ["int", ["string"]],

    NFSE_ConfigLerValor: ["int", ["string", "string", "string", "string"]],

    NFSE_ConfigGravarValor: ["int", ["string", "string", "string"]],

    NFSE_ConfigImportar: ["int", ["string"]],

    NFSE_ConfigExportar: ["int", ["string", "string"]],

    NFSE_CarregarXML: ["int", ["string"]],

    NFSE_CarregarINI: ["int", ["string"]],

    NFSE_ObterXml: ["int", ["int", "string", "string"]],

    NFSE_GravarXml: ["int", ["int", "string", "string"]],

    NFSE_ObterIni: ["int", ["int", "string", "string"]],

    NFSE_GravarIni: ["int", ["int", "string", "string"]],

    NFSE_LimparLista: ["int", []],

    NFSE_ObterCertificados: ["int", ["string", "string"]],

    NFSE_Cancelar: [
      "int",
      ["string", "string", "string", "int", "string", "string"],
    ],

    NFSE_EnviarEvento: ["int", ["int", "string", "string"]],

    NFSE_EnviarEmail: [
      "int",
      ["string", "string", "bool", "string", "string", "string", "string"],
    ],

    NFSE_Imprimir: [
      "int",
      ["string", "int", "string", "string", "string", "string", "string"],
    ],

    NFSE_ImprimirPDF: ["int", []],
  });

  let inicio = 2;
  const buflength = 4096;

  let aloc_sResposta = Buffer.alloc(buflength);
  let aloc_esTamanho = ref.alloc("int", buflength);

  console.log(`Iniciando >>>>>>>`);
  inicio = libm.NFSE_Inicializar(eArqConfig, eChaveCrypt);
  console.log(`iniciou >>>>>>> ${inicio}`);

  inicio = libm.NFSE_ConfigGravarValor("DFe", "ArquivoPFX", pathCert);
  console.log(`Set Configurações ArquivoPFX ${inicio}`);

  inicio = libm.NFSE_ConfigGravarValor("DFe", "Senha", '1234');
  console.log(`Set Configurações sENHA ${inicio}`);

  inicio = libm.NFSE_ConfigGravarValor("NFSe", "PathSchemas", pathSchemas);
  console.log(`Set Configurações PathSchemas ${inicio}`);

  inicio = libm.NFSE_ConfigGravarValor("NFSe", "IniServicos", pathIniSchemas);
  console.log(`Set Configurações IniServicos ${inicio}`);

  aloc_sResposta = Buffer.alloc(buflength);
  aloc_esTamanho = ref.alloc("int", buflength);
  inicio = libm.NFSE_Emitir("55", 0, false, aloc_sResposta, aloc_esTamanho);

  console.log(`Mensagem: `, aloc_sResposta.toString());
  console.log(`NFSE_Emitir >>>>>>>> ${inicio}`);

  inicio = libm.NFSE_Finalizar();
  console.log(`finalizar >>>>>>>> ${inicio}`);
}

getNFe();
