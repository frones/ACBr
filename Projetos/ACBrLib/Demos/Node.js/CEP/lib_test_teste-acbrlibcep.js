const path = require('path');
const ffi = require('ffi-napi');
const ref = require('ref-napi');

const pathDllACBrLibCEP = path.join(__dirname, 'ACBrCEP64.dll')
var eArqConfig = path.join(__dirname, 'ACBrLib.ini');
var eChaveCrypt = '';


var tint = ref.refType('int');
var tchar = ref.refType('string');

var lib = ffi.Library(pathDllACBrLibCEP, {
    CEP_Inicializar: ['int', ['string', 'string']],
    CEP_Finalizar: ['int', []],
    CEP_BuscarPorCEP: ['int', ['string', tchar, tint]],
})

try {

    var inicio = 2;
    const buflength = 256;

    let aloc_sResposta = Buffer.alloc(buflength);
    let aloc_esTamanho = ref.alloc('int', buflength);



    inicio = lib.CEP_Inicializar(eArqConfig, eChaveCrypt);
    console.log(`iniciou >>>>>>> ${inicio}`);

    inicio = lib.CEP_BuscarPorCEP('18270-170', aloc_sResposta, aloc_esTamanho);
    console.log(`ultmio retorno >>>>>>>> ${inicio}`);
    console.log(`Retorno: `, aloc_sResposta.toString());


    inicio = lib.CEP_Finalizar();
    console.log(`finalizar >>>>>>>> ${inicio}`);


} catch (error) {
    console.error('An error occurred:', error)
}

