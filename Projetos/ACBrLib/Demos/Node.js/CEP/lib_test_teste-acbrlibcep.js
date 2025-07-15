const path = require('path');
const koffi = require('koffi');

const pathDllACBrLibCEP = path.join(__dirname, 'ACBrCEP64.dll')
var eArqConfig = path.join(__dirname, 'ACBrLib.ini');
var eChaveCrypt = '';

const acbrcep = koffi.load(pathDllACBrLibCEP)
const TAMANHO_BUFFER = 1024;
const lib = {
    CEP_Inicializar: acbrcep.func("CEP_Inicializar", 'int', ['string', 'string']),
    CEP_ConfigGravarValor: acbrcep.func("CEP_ConfigGravarValor", 'int', ['string', 'string', 'string']),
    CEP_ConfigGravar: acbrcep.func("CEP_ConfigGravar", 'int', ['string']),
    CEP_UltimoRetorno: acbrcep.func("CEP_UltimoRetorno", 'int', ['char*', 'int*']),
    CEP_Nome: acbrcep.func("CEP_Nome", 'int', ['char*', 'int*']),
    CEP_Versao: acbrcep.func("CEP_Versao", 'int', ['char*', 'int*']),
    CEP_ConfigLer: acbrcep.func("CEP_ConfigLer", 'int', ['string']),
    CEP_ConfigLerValor: acbrcep.func("CEP_ConfigLerValor", 'int', ['string', 'string', 'char*', 'int*']),
    CEP_Finalizar:  acbrcep.func("CEP_Finalizar", 'int', []),
    CEP_BuscarPorCEP: acbrcep.func("CEP_BuscarPorCEP", 'int', ['string', 'char*', 'int*'])
}

try {

    var inicio = 2;

    let aloc_sResposta = Buffer.alloc(TAMANHO_BUFFER);
    let aloc_esTamanho = koffi.alloc('int', 1);

    //configura tamanho do buffer
    koffi.encode(aloc_esTamanho, 'int', TAMANHO_BUFFER)


    inicio = lib.CEP_Inicializar(eArqConfig, eChaveCrypt);
    console.log(`iniciou >>>>>>> ${inicio}`);

    //configura biblioteca
    lib.CEP_ConfigGravarValor('Principal','LogPath',__dirname)
    lib.CEP_ConfigGravarValor('Principal','LogNivel','4')
    lib.CEP_ConfigGravarValor('CEP','WebService','10')
    lib.CEP_ConfigGravar(eArqConfig)
    

    inicio = lib.CEP_BuscarPorCEP('18270-170', aloc_sResposta, aloc_esTamanho);
    console.log(`ultmio retorno >>>>>>>> ${inicio}`);
    console.log(`Retorno: `, aloc_sResposta.toString());


    inicio = lib.CEP_Finalizar();
    console.log(`finalizar >>>>>>>> ${inicio}`);


} catch (error) {
    console.error('An error occurred:', error)
}

