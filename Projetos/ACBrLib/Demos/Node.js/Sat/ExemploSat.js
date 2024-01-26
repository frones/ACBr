const path = require('path');
const ffi = require('ffi-napi');
const ref = require('ref-napi');

//Lib a ser carregada
const pathDllACBrLibSat = path.join(__dirname, 'ACBrSAT64.dll')

//DLL do fabricante (compativel com sua compilacao x86/x64)
const pathDllSat = path.join(__dirname, 'dllsat.dll')

const pathLogX = path.join(__dirname, '\\log')
var eArqConfig = path.join(__dirname, 'ACBrLib.ini');
var eChaveCrypt = '';


var tint = ref.refType('int');
var tchar = ref.refType('string');

var lib = ffi.Library(pathDllACBrLibSat, {
    SAT_Inicializar: ['int', ['string', 'string']],
    SAT_InicializarSAT: ['int', []],
    SAT_Finalizar: ['int', []],
    SAT_DesInicializar: ['int', []],
    SAT_ConsultarSAT: ['int', ['string', 'string']],
    SAT_ConfigLer: ['int', [ 'string']],
    SAT_ConfigGravar: ['int', [ 'string']],
    SAT_ConfigLerValor: ['int',['string','string','string','string']],
    SAT_ConfigGravarValor: ['int', [ 'string', 'string', 'string']],
    SAT_ConfigImportar: ['int',[ 'string']],
    SAT_ConfigExportar: ['int',[ 'string','string']],
    SAT_ImprimirExtratoVenda: ['int', [ 'string']],
})

try {

    var inicio = 2;
    const buflength = 1096;
    console.log(`Iniciar Biblioteca >>>>>>> ${inicio}`);

    let aloc_sResposta = Buffer.alloc(buflength);
    let aloc_esTamanho = ref.alloc('int', buflength);

    inicio = lib.SAT_Inicializar(eArqConfig, eChaveCrypt);
    console.log(`Pasta Trabalho >>>>>>> ${__dirname}`);

    console.log('- Configurando Log ');
    lib.SAT_ConfigGravarValor('Principal', 'TipoResposta', '2');
    lib.SAT_ConfigGravarValor('Principal', 'LogNivel', '4');
    lib.SAT_ConfigGravarValor('Principal', 'LogPath', pathLogX);     
    lib.SAT_ConfigGravarValor('Sat', 'NomeDLL', pathDllSat);     

    inicio = lib.SAT_InicializarSAT();
    console.log(`1.) SAT_InicializarSAT - Inicializa a comunicação com o SAT: ${inicio}`);

    inicio = lib.SAT_ConsultarSAT(aloc_sResposta, aloc_esTamanho);
    console.log(`2.) SAT_ConsultarSAT - retorna o status do SAT: ${inicio}`);
    console.log(`Resposta: `, aloc_sResposta.toString());

    //Informar o camiminho completo do xml a ser impresso e descomentar a linha abaixo
    //inicio = lib.SAT_ImprimirExtratoVenda('D:\\ExemploNode\\SAT\\AD41240127101611000182599000198240001466080908.xml');
    //console.log(`3.)  SAT_ImprimirExtratodeVenda - Imprime Extrato XML:  ${inicio}`);

    inicio = lib.SAT_DesInicializar();
    console.log(`4.) SAT_DesInicializar - Finaliza a comunicação com o SAT: ${inicio}`);


    inicio = lib.SAT_Finalizar();
    console.log(`5.) SAT_Finalizar - remove o componente ACBrLibSAT da memoria: ${inicio}`);


} catch (error) {
    console.error('An error occurred:', error)
}

