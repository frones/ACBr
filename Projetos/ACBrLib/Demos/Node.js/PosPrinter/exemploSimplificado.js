const ffi = require('ffi-napi');
const ref = require('ref-napi');
const path = require('path');

/*
----------------------------------------------------------------------------
    Atenção - Apos a primeira execução vai ser criado acbrlib.ini
    Necessario configurar dados da impressora, exemplo:

[PosPrinter]
ArqLog=log.txt
Modelo=1
Porta=USB
PaginaDeCodigo=2
ColunasFonteNormal=48
EspacoEntreLinhas=0
LinhasEntreCupons=6
CortaPapel=1
TraduzirTags=1    
    
----------------------------------------------------------------------------
*/

// Caminho para a DLL e o arquivo INI
const dllPath = path.resolve('C:\\ACBr\\Projetos\\ACBrLib\\Demos\\Node.js\\PosPrinter\\ACBrPosPrinter64.dll');
const iniPath = path.resolve('C:\\ACBr\\Projetos\\ACBrLib\\Demos\\Node.js\\PosPrinter\\ACBrLIB.ini');

// Carrega a DLL usando ffi
const acbrLib = ffi.Library(dllPath, {
    'POS_Inicializar': ['int', ['string', 'string']],
    'POS_Ativar': ['int', []],
    //'POS_LerStatusImpressora': ['int', ['string', 'pointer']],
    'POS_Desativar': ['int', []],
    'POS_Finalizar': ['int', []],
    'POS_Imprimir':['int',['string','int','int','int','int']]
});

// Inicializar a impressora
const resultadoInicializar = acbrLib.POS_Inicializar(iniPath, '');
if (resultadoInicializar == 0) {
    console.log('A Lib foi inicializada com sucesso, código:', resultadoInicializar);
} else {
    console.log('Erro ao inicializar a Lib, código:', resultadoInicializar);
}

// Texto de exemplo
const texto = `
</zera>
</linha_dupla>
FONTE NORMAL: 48 Colunas
....+....1....+....2....+....3....+....4....+...
<e>EXPANDIDO: 24 Colunas
....+....1....+....2....
</e><c>CONDENSADO: 64 Colunas
....+....1....+....2....+....3....+....4....+....5....+....6....
</c><n>FONTE NEGRITO</N>
<in>FONTE INVERTIDA</in>
<S>FONTE SUBLINHADA</s>
<i>FONTE ITALICO</i>
FONTE NORMAL
</linha_simples>
<n>LIGA NEGRITO
<i>LIGA ITALICO
<S>LIGA SUBLINHADA
<c>LIGA CONDENSADA
<e>LIGA EXPANDIDA
<a>LIGA ALTURA DUPLA
</fn>FONTE NORMAL
</linha_simples>
<e><n>NEGRITO E EXPANDIDA</n></e>
<c><n>NEGRITO E CONDENSADA</n></c>
<e><a>EXPANDIDA E ALT.DUPLA</a></e>
</fn>FONTE NORMAL
<in><e>INVERTIDA E EXPANDIDA</e></in>
<in><c>INVERTIDA E CONDENSADA</c></in>
<in><a>INVERTIDA E ALT.DUPLA</a></in>
</fn>FONTE NORMAL
</linha_simples>
</fb>FONTE TIPO B
</fn><n>FONTE NEGRITO</N>
<e>FONTE EXPANDIDA</e>
<a>FONTE ALT.DUPLA</a>
<in>FONTE INVERTIDA</in>
<S>FONTE SUBLINHADA</s>
<i>FONTE ITALICO</i>
</FA>FONTE TIPO A
</FN>FONTE NORMAL
</corte_total>

`;

// Ativar impressora
const resultadoAtivar = acbrLib.POS_Ativar();
if (resultadoAtivar == 0) {
    console.log('Impressora foi ativada com sucesso, código:', resultadoAtivar);
} else {
    console.log('Ocorreu um erro ao ativar a impressora, código:', resultadoAtivar);
}

// Imprimir
const retornoImprimir = acbrLib.POS_Imprimir(texto,1,1,1,1);
if (retornoImprimir == 0) {
    console.log('O texto foi impresso com sucesso, código:', retornoImprimir);
} else {
    console.log('Erro ao imprimir o texto, código:', retornoImprimir);
}

// Desativar e finalizar a impressora
acbrLib.POS_Desativar();
acbrLib.POS_Finalizar();
