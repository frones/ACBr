{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBematech ;

interface
uses Classes,
   ACBrECFClass, ACBrDevice, ACBrCHQClass;

const ErrosST1 : array[0..7] of string =
      ('Número de parâmetros de CMD inválido',
       'Cupom aberto',
       'Comando inexistente',
       'Primeiro dado de CMD não foi ESC (27d)',
       'Impressora em erro',
       'Erro no relógio',
       'Pouco papel',
       'Fim de papel') ;
const ErrosST2 : array[0..7] of string =
      ('Comando não executado',
       'CGC/IE do proprietário não programados',
       'Cancelamento não permitido',
       'Capacidade de alíquotas programáveis lotada',
       'Alíquota não programada',
       'Erro na Memória RAM CMOS não volátil',
       'Memória fiscal lotada',
       'Tipo de parâmetro de CMD inválido') ;

const
 // Usada apenas para passar para as DLLs. Valor não precisa ser válido,
 // o próprio ACBrECF assinará o arquivo
 cChavePublica = 'A499F300F731F6892F44B83A5DD9D97CFFFD0ABE96E29B4B4B4'+
                 'EB2F9E5BCFFCF0A52EAFDF05779F90B3A199BE5776B13373CB2'+
                 'E71D8AB67F4080CE27B226FFF032B6A7182C90C935EF2F4D343'+
                 'A743B60307EE4961F0C5EB02B1CEEF48D647C02E9BE164DC404'+
                 'B833F80C5B4268C04039547E7D5E242537B02360674B569208BD';

 cChavePrivada = 'D19598300478932ACFFE16CB6903552F15FDBD2D3B9659FAD79'+
                 'C3603C07B875919E9D8B28919B8F4C20C6AE23268A636D1206F'+
                 '5E6BC79D89B6152804B15A9781C90E0A2D5064FB5B7CC01048A'+
                 'D8C66768F76D71647E7D39F8EDD714044CEA68F2A4010684913'+
                 '2B01D14DDEB3FBA6FC1A9FBE9EA71BAB9293707A4EAD29CB6F3D';


  ErrosST3: array [0 .. 218] of string = (
    { 0 } 'Comando ok',
    { 1 } 'Comando inválido',
    { 2 } 'Erro desconhecido',
    { 3 } 'Número de parâmetro inválido',
    { 4 } 'Tipo de parâmetro inválido',
    { 5 } 'Todas alíquotas já programadas',
    { 6 } 'Totalizador não fiscal já programado',
    { 7 } 'Cupom fiscal aberto',
    { 8 } 'Cupom fiscal fechado',
    { 9 } 'ECF ocupado',
    { 10 } 'Impressora em erro',
    { 11 } 'Impressora sem papel',
    { 12 } 'Impressora com cabeça levantada',
    { 13 } 'Impressora off line',
    { 14 } 'Alíquota não programada',
    { 15 } 'Terminador de string faltando',
    { 16 } 'Acréscimo ou desconto maior que o total do cupom fiscal',
    { 17 } 'Cupom fiscal sem item vendido',
    { 18 } 'Comando não efetivado',
    { 19 } 'Sem espaço para novas formas de pagamento',
    { 20 } 'Forma de pagamento não programada',
    { 21 } 'Índice maior que número de forma de pagamento',
    { 22 } 'Formas de pagamento encerradas',
    { 23 } 'Cupom não totalizado',
    { 24 } 'Comando maior que 7f',
    { 25 } 'Cupom fiscal aberto e sem item',
    { 26 } 'Cancelamento não imediatamente após',
    { 27 } 'Cancelamento já efetuado',
    { 28 } 'Comprovante de crédito ou débito não permitido ou já emitido',
    { 29 } 'Meio de pagamento não permite tef',
    { 30 } 'Sem comprovante não fiscal aberto',
    { 31 } 'Comprovante de crédito ou débito já aberto',
    { 32 } 'Reimpressão não permitida',
    { 33 } 'Comprovante não fiscal já aberto',
    { 34 } 'Totalizador não fiscal não programado',
    { 35 } 'Cupom não fiscal sem item vendido',
    { 36 } 'Acréscimo e desconto maior que total cnf',
    { 37 } 'Meio de pagamento não indicado',
    { 38 } 'Meio de pagamento diferente do total de recebimento',
    { 39 } 'Não permitido mais de uma sangria ou suprimento',
    { 40 } 'Relatório já programado',
    { 41 } 'Relatório gerencial não programado',
    { 42 } 'Relatório gerencial não permitido',
    { 43 } 'Mfd não inicializada',
    { 44 } 'Mfd ausente',
    { 45 } 'Mfd sem número de série',
    { 46 } 'Mfd já inicializada',
    { 47 } 'Mfd lotada',
    { 48 } 'Cupom não fiscal aberto',
    { 49 } 'Memória fiscal desconectada',
    { 50 } 'Memória fiscal sem número de série da mfd',
    { 51 } 'Memória fiscal lotada',
    { 52 } 'Data inicial inválida',
    { 53 } 'Data final inválida',
    { 54 } 'Contador de redução z inicial inválido',
    { 55 } 'Contador de redução z final inválido',
    { 56 } 'Erro de alocação',
    { 57 } 'Dados do RTC incorretos',
    { 58 } 'Data anterior ao último documento emitido',
    { 59 } 'Fora de intervenção técnica',
    { 60 } 'Em intervenção técnica',
    { 61 } 'Erro na memória de trabalho',
    { 62 } 'Já houve movimento no dia',
    { 63 } 'Bloqueio por RZ',
    { 64 } 'Forma de pagamento aberta',
    { 65 } 'Aguardando primeiro proprietário',
    { 66 } 'Aguardando RZ',
    { 67 } 'Ecf ou loja igual a zero',
    { 68 } 'Cupom adicional não permitido',
    { 69 } 'Desconto maior que total vendido em ICMS',
    { 70 } 'Recebimento não fiscal nulo não permitido',
    { 71 } 'Acréscimo ou desconto maior que total não fiscal',
    { 72 } 'Memória fiscal lotada para novo cartucho',
    { 73 } 'Erro de gravação na MF',
    { 74 } 'Erro de gravação na MFD',
    { 75 } 'Dados do RTC anteriores ao último documento armazenado',
    { 76 } 'Memória fiscal sem espaço para gravar leituras da MFD',
    { 77 } 'Memória fiscal sem espaço para gravar versão do SB',
    { 78 } 'Descrição igual a default não permitido',
    { 79 } 'Extrapolado número de repetições permitidas',
    { 80 } 'Segunda via do comprovante de crédito ou débito não permitido',
    { 81 } 'Parcelamento fora da sequência',
    { 82 } 'Comprovante de crédito ou débito aberto',
    { 83 } 'Texto com sequência de ESC inválida',
    { 84 } 'Texto com sequência de ESC incompleta',
    { 85 } 'Venda com valor nulo',
    { 86 } 'Estorno de valor nulo',
    { 87 } 'Forma de pagamento diferente do total da sangria',
    { 88 } 'Redução não permitida em intervenção técnica',
    { 89 } 'Aguardando RZ para entrada em intervenção técnica',
    { 90 } 'Forma de pagamento com valor nulo não permitido',
    { 91 } 'Acréscimo e desconto maior que valor do item',
    { 92 } 'Autenticação não permitida',
    { 93 } 'Timeout na validação',
    { 94 } 'Comando não executado em impressora bilhete de passagem',
    { 95 } 'Comando não executado em impressora de Cupom Fiscal',
    { 96 } 'Cupom Não Fiscal fechado',
    { 97 } 'Parâmetro não ASCII em campo ASCII',
    { 98 } 'Parâmetro não ASCII num em campo ASCII num',
    { 99 } 'Tipo de transporte inválido',
    { 100 } 'Data e hora inválida',
    { 101 } 'Sem relatório gerencial',
    { 102 } 'Número do totalizador não fiscal inválido',
    { 103 } 'Parâmetro de acréscimo ou desconto inválido',
    { 104 } 'Acréscimo ou desconto em sangria ou suprimento não permitido',
    { 105 } 'Número do relatório gerencial inválido',
    { 106 } 'Forma de pagamento fonte não programada',
    { 107 } 'Forma de pagamento destino não programada',
    { 108 } 'Estorno maior que forma pagamento',
    { 109 } 'Caracter numérico na codificação GT não permitido',
    { 110 } 'Erro na inicialização da MF',
    { 111 } 'Nome do totalizador em branco não permitido',
    { 112 } 'Data e hora anteriores ao último doc armazenado',
    { 113 } 'Parâmetro de acréscimo ou desconto inválido',
    { 114 } 'Item anterior aos trezentos últimos',
    { 115 } 'Item não existe ou já cancelado',
    { 116 } 'Código com espaços não permitido',
    { 117 } 'Descrição sem caracter alfabético não permitido',
    { 118 } 'Acréscimo maior que valor do item',
    { 119 } 'Desconto maior que valor do item',
    { 120 } 'Desconto em iss não permitido',
    { 121 } 'Acréscimo em item já efetuado',
    { 122 } 'Desconto em item já efetuado',
    { 123 } 'Erro na Memória Fiscal chamar credenciado',
    { 124 } 'Aguardando gravação na Memória Fiscal',
    { 125 } 'Caracter repetido na codificação do GT',
    { 126 } 'Versão já gravada na memória fiscal',
    { 127 } 'Estouro de capacidade no cheque',
    { 128 } 'Timeout na leitura do cheque',
    { 129 } 'Mês inválido',
    { 130 } 'Coordenada inválida',
    { 131 } 'Sobreposição de texto',
    { 132 } 'Sobreposição de texto no valor',
    { 133 } 'Sobreposição de texto no extenso',
    { 134 } 'Sobreposição de texto no favorecido',
    { 135 } 'Sobreposição de texto na localidade',
    { 136 } 'Sobreposição de texto no opcional',
    { 137 } 'Sobreposição de texto no dia',
    { 138 } 'Sobreposição de texto no mês',
    { 139 } 'Sobreposição de texto no ano',
    { 140 } 'Usando MFD de outro ECF',
    { 141 } 'Primeiro dado diferente de ESC ou 1C',
    { 142 } 'Não permitido alterar sem intervenção técnica',
    { 143 } 'Dados da última RZ corrompidos',
    { 144 } 'Comando não permitido no modo inicialização',
    { 145 } 'Aguardando acerto de relógio',
    { 146 } 'MFD já inicializada para outra memória fiscal',
    { 147 } 'Aguardando acerto do relógio ou desbloqueio pelo teclado',
    { 148 } 'Valor forma de pagamento maior que máximo permitido',
    { 149 } 'Razão social em branco',
    { 150 } 'Nome de fantasia em branco',
    { 151 } 'Endereço em branco',
    { 152 } 'Estorno de CDC não permitido',
    { 153 } 'Dados do proprietário iguais ao atual',
    { 154 } 'Estorno de forma de pagamento não permitido',
    { 155 } 'Descrição forma de pagamento igual já programada',
    { 156 } 'Acerto de horário de verão só imediatamente após Redução Z',
    { 157 } 'Intervenção técnica não permitida MF reservada para Redução Z',
    { 158 } 'Senha CNPJ inválida',
    { 159 } 'Timeout na inicialização de nova MF',
    { 160 } 'Dados da MFD não encontrados',
    { 161 } 'Sangria ou Suprimento devem ser únicos no comprovante não fiscal',
    { 162 } 'Índice da forma de pagamento nulo não permitido',
    { 163 } 'UF de destino inválida',
    { 164 } 'Tipo de transporte incompatível com a UF de destino',
    { 165 } 'Descrição do primeiro item do Bilhete de passagem diferente de “TARIFA”',
    { 166 } 'Aguardando impressão de cheque ou autenticação166  Aguardando impressão de cheque ou autenticação',
    { 167 } 'Programação de CNPJ e IE preenchido com espaços em branco não permitido',
    { 168 } 'Número de impressões da fita detalhe nesta intervenção técnica esgotado',
    { 169 } 'Cupom fiscal já sub-totalizado',
    { 170 } 'Cupom não sub-totalizado',
    { 171 } 'Acréscimo em sub-total já efetuado',
    { 172 } 'Desconto em sub-total já efetuado',
    { 173 } 'Acréscimo com valor nulo não permitido',
    { 174 } 'Desconto com valor nulo não permitido',
    { 175 } 'Cancelamento de acréscimo ou desconto em sub-total não permitido',
    { 176 } 'Data inválida',
    { 177 } 'Valor do cheque nulo não permitido',
    { 178 } 'Valor do cheque inválido',
    { 179 } 'Cheque sem a informação da localidade não permitido',
    { 180 } 'Cancelamento de acréscimo em item não permitido',
    { 181 } 'Cancelamento de desconto em item não permitido',
    { 182 } 'Número máximo de itens atingido',
    { 183 } 'Número do item nulo não permitido',
    { 184 } 'Mais que duas alíquotas diferentes no Bilhete de Passagem não permitido',
    { 185 } 'Acréscimo ou Desconto em item não permitido',
    { 186 } 'Cancelamento de acréscimo ou desconto em item não permitido',
    { 187 } 'Clichê já impresso',
    { 188 } 'Texto opcional do cheque excedeu o máximo de linhas permitido',
    { 189 } 'Impressão automática no verso não disponível neste equipamento',
    { 190 } 'Timeout na inserção do cheque',
    { 191 } 'Overflow na capacidade de texto do comprovante de débito ou crédito',
    { 192 } 'Programação de espaços entre cupons menor que o mínimo permitido',
    { 193 } 'Equipamento não possui leitor de cheque',
    { 194 } 'Programação de alíquota com valor nulo não permitido',
    { 195 } 'Erro nao documentado',
    { 196 } 'Parâmetro Baud Rate inválido',
    { 197 } 'Configuração permitida somente pela porta do fisco',
    { 198 } 'Valor total do item excede 11 dígitos',
    { 199 } 'Programação da moeda com espaços em branco não permitido',
    { 200 } 'Casa decimais devem ser programadas com 2 ou 3',
    { 201 } 'Erro não documento',
    { 202 } 'Identificação do consumidor não permitido para sangria ou suprimento',
    { 203 } 'Casas decimais em quantidade maior que permitida',
    { 204 } 'Casas decimais do unitário maior que a permitida',
    { 205 } 'Posição reservada para ICMS',
    { 206 } 'Posição reservada para ISSQN',
    { 207 } 'Todas as alíquotas com a mesma vinculação não permitida',
    { 208 } 'Data de embarque anterior à data de emissão',
    { 209 } 'Alíquota para ISSQN não permitida sem inicialização da Inscrição Municipal',
    { 210 } 'Pacote do clichê gráfico fora da seqüência',
    { 211 } 'Espaço para armazenamento do clichê gráfico esgotado',
    { 212 } 'Clichê gráfico não disponível para confirmação',
    { 213 } 'CRC do clichê gráfico diferente do informado',
    { 214 } 'Intervalo inválido',
    { 215 } 'Usuário já programado',
    { 216 } 'Troca de MFD não danificada não permitida',
    { 217 } 'Detectada abertura do equipamento',
    { 218 } 'Cancelamento de Acréscimo/Desconto não permitido');

{$IFNDEF MSWINDOWS}
 cLIB_VersaoMinima = '3.21.0';
{$ELSE}
 cLIB_VersaoMinima = '6.1.1.6';
{$ENDIF}

{$IFNDEF MSWINDOWS}
 cLIB_Bema = 'libbemafiscal.so';
{$ELSE}
 {$IFDEF CPU64}
  cLIB_Bema = 'Bemafi64.dll';
 {$ELSE}
  cLIB_Bema = 'Bemafi32.dll';
 {$ENDIF}
{$ENDIF}

type

{ Classe filha de TACBrECFClass com implementaçao para Bematech }

{ TACBrECFBematech }

TACBrECFBematech = class( TACBrECFClass )
 private
    fsACK, fsST1, fsST2, fsST3: Integer ; { Status da Bematech }
    fsProp: AnsiString;
    { Tamanho da Resposta Esperada ao comando. Necessário, pois a Bematech nao
      usa um Sufixo padrão no fim da resposta da Impressora. }
    fs25MFD      : Boolean ;  // True se for MP25 ou Superior (MFD)
    fsPAF        : String ;
    fsBytesResp  : Integer ;
    fsFalhasFimImpressao : Integer ;
    fsNumVersao : String ;
    fsNumECF    : String ;
    fsNumLoja   : String ;
    fsNumCRO    : String ;
    fsSubModeloECF  : String ;
    fsNumCOOInicial : String ;
    fsArredonda : Char ;
    fsTotalPago : Double ;
    fsTotalizadoresParciais : String ;
    fsNFCodCNF, fsNFCodFPG : String ;
    fsNFValor : Double ;

    fsModelosCheque : TACBrCHQModelos ;

    xBematech_FI_ReloadINIFile:Function:Integer;{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_AbrePortaSerial:Function:Integer;{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_VersaoDll:Function( cVersao: AnsiString ): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_FechaPortaSerial:Function:Integer;{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_EspelhoMFD:Function( cNomeArquivoDestino: AnsiString;
       cDadoInicial: AnsiString; cDadoFinal: AnsiString;
       cTipoDownload: AnsiString; cUsuario: AnsiString;
       cChavePublica: AnsiString; cChavePrivada: AnsiString ):Integer;{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_ArquivoMFDPath:Function( cNomeArquivoOrigem: AnsiString;
       cNomeArquivoDestino: AnsiString;
       cDadoInicial: AnsiString; cDadoFinal: AnsiString;
       cTipoDownload: AnsiString; cUsuario: AnsiString;
       iTipoGeracao: integer; cChavePublica: AnsiString;
       cChavePrivada: AnsiString; iUnicoArquivo: integer ):Integer;{$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_GeraRegistrosCAT52MFD: function(cNomeArquivoMFD: AnsiString;
       cData: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_DownloadMFD: function(cNomeArquivoMFD, cTipoDownload,
      cDadoInicial, cDadoFinal, cUsuario: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_DownloadMF: function(cNomeArquivoMF: AnsiString): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_RelatorioSintegraMFD: function (iRelatorios : Integer;
                                                 cArquivo    : String;
                                                 cMes        : String;
                                                 cAno        : String;
                                                 cRazaoSocial: String;
                                                 cEndereco   : String;
                                                 cNumero     : String;
                                                 cComplemento: String;
                                                 cBairro     : String;
                                                 cCidade     : String;
                                                 cCEP        : String;
                                                 cTelefone   : String;
                                                 cFax        : String;
                                                 cContato    : String): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
    xBematech_FI_GeraRegistrosSpedCompleto: function (cArquivoMFD: string;
                                                      cArquivoTXT: string;
                                                      cDataInicial: string;
                                                      cDataFinal: string;
                                                      cPerfil: string;
                                                      cCFOP: string;
                                                      cCODOBSFiscal: string;
                                                      cAliqPIS: string;
                                                      cAliqCOFINS: string;
                                                      cEmpresa: string;
                                                      cCodMunicipio: string): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
     procedure LoadDLLFunctions;
     procedure UnLoadDLLFunctions;
     procedure AbrePortaSerialDLL(const aPath: String='');
     procedure FechaPortaSerialDLL(const OldAtivo : Boolean) ;
     function AnalisarRetornoDll(const ARetorno: Integer): String;

    function GetProp: AnsiString;
    function GetTotalizadoresParciais : String ;
    procedure FinalidadeToTipoPrefixo( AFinalidade : TACBrECFFinalizaArqMFD;
       var Tipo: Integer; var Prefixo: AnsiString) ;
    function ACKValido( const nACK: Integer): Boolean;

 protected
    property TotalizadoresParciais : String read GetTotalizadoresParciais ;
    procedure CRZToCOO(const ACRZIni, ACRZFim: Integer; var ACOOIni, ACOOFim: Integer); //isso aqui ele diz que não está sendo utilizado.

    Function PreparaCmd( cmd : AnsiString ) : AnsiString ;

    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumECF: String; override ;
    function GetNumLoja: String; override ;
    function GetNumSerie: String; override ;
    function GetNumSerieMFD: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetGavetaAberta: Boolean; override ;
    function GetPoucoPapel : Boolean; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetParamDescontoISSQN: Boolean; override ;
    function GetArredonda: Boolean; override ;
    function GetChequePronto: Boolean; override ;

    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetIM: String; override ;
    function GetCliche: AnsiString; override ;
    function GetUsuarioAtual: String; override ;
    function GetDataHoraSB: TDateTime; override ;
    function GetSubModeloECF: String ; override ;

    function GetPAF: String; override ;
    function GetDataMovimento: TDateTime; override ;
    function GetDataHoraUltimaReducaoZ : TDateTime ; override ;
    function GetGrandeTotal: Double; override ;
    function GetNumCRO: String; override ;
    function GetNumCCF: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetNumCFC: String; override ;
    function GetNumGNFC: String; override ;
    function GetNumCFD: String; override ;
    function GetNumCRZ: String; override ;
    function GetNumNCN: String; override ;
    function GetNumCCDC: String; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalTroco: Double; override ;
    function GetNumReducoesZRestantes: String; override ;

    function GetTotalAcrescimos: Double; override ;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalDescontos: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetTotalIsencao: Double; override ;

    function GetTotalAcrescimosISSQN: Double; override;
    function GetTotalCancelamentosISSQN: Double; override;
    function GetTotalDescontosISSQN: Double; override;
    function GetTotalSubstituicaoTributariaISSQN: Double; override;
    function GetTotalNaoTributadoISSQN: Double; override;
    function GetTotalIsencaoISSQN: Double; override;

    function GetTotalAcrescimosOPNF: Double; override;
    function GetTotalCancelamentosOPNF: Double; override;
    function GetTotalDescontosOPNF: Double; override;

    function GetNumCOOInicial: String; override ;
    function GetNumUltimoItem: Integer; override ;

    function GetDadosUltimaReducaoZ: String; override ;

    Function VerificaFimLeitura(var Retorno: AnsiString;
       var TempoLimite: TDateTime) : Boolean ; override ;
    function VerificaFimImpressao(var TempoLimite: TDateTime) : Boolean ; override ;
 public
    Constructor create( AOwner : TComponent  )  ;
    Destructor Destroy  ; override ;

    procedure Ativar ; override ;

    Property ACK   : Integer read fsACK ;
    Property ST1   : Integer read fsST1 ;
    Property ST2   : Integer read fsST2 ;
    property ST3   : Integer read fsST3 ;
    property ModelosCheque : TACBrCHQModelos read fsModelosCheque
       write fsModelosCheque ;
    property Prop  : AnsiString read GetProp write fsProp;

    Property BytesResp : Integer read fsBytesResp write fsBytesResp ;
    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; override ;

    Procedure AbreCupom ; override ;
    procedure AbreBilhetePassagem(Origem: string; Destino: string;
      Linha: string; Agencia: string; DataHora: TDateTime; Poltrona: string;
      Plataforma: string; Tipo: TACBrECFTipoBilhete; UFDestino: string;
      PassageiroRG: string; PassageiroNome: string; PassageiroEnd: string); override;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    procedure VendeItemEx( Codigo, Descricao : String; AliquotaICMS : String;
           Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
           Unidade : String = 'UN'; TipoDescontoAcrescimo : String = '%';
           DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1;
           EAN13: String = '';              // Código Barras do Produto (GTIN-13)
           CasasDecimaisQtde: Integer = 0;  // Se 0 assume o valor de DecimaisQtd
           CasasDecimaisValor: Integer = 0; // Se 0 assume o valor de DecimaisPreco
           ArredondaTrunca: Char = 'A';     // Se diferente de 'A' ou 'T' assume o valor de "Arredonda"
           NCM: String = '';                // Código da Nomenclatura Comum do MERCOSUL
           CFOP: String = '';               // Código Fiscal de Operações e Prestações
           InformacaoAdicional: String = '';// Texto Livro, até 500 caracteres
           TotalDosTributos: Double = 0;    // Valor da lei "De olho no Imposto)
           OrigemProduto: Integer = 0;      // 0–Nacional; 1–Estrangeira Import.direta; 2–Estrangeira–Mercado interno

           CST_ICMS: String = '';           // ICMS: Código de Situação Tributária
           ModalidadeBCICMS: Integer = 0;   // ICMS: Modalidade Base de Calculo: 0 – Margem do valor agregado (%)
                                            //                                   1 – Pauta (Valor)
                                            //                                   2 – Preço tabelado máx. (Valor)
                                            //                                   3 – Valor da operação
           PercentualReducaoBCICMS: Double = 0; // ICMS:
           CSOSN: String = '';                  // Simples Nacional: Código de Situação da Operação
           ValorBaseCalculoSN: Double = 0;      // Simples Nacional: Base de Calculo
           ValorICMSRetidoSN: Double = 0;       // Simples Nacional: Valor Retido para ICMS
           AliquotaCalculoCreditoSN: Double = 0;// Simples Nacional:
           ValorCreditoICMSSN: Double = 0;      // Simples Nacional:
           ItemListaServico: String = '';   // Serviço apenas: código do serviço prestado: lista de serviços anexa à Lei Complementar nº 116,
           CodigoISS: String = '';          // Serviço apenas: Código do Imposto Sobre Serviço
           NaturezaOperacaoISS: String = '';// Serviço apenas: com os seguintes valores possíveis: '00' até '08',
           IndicadorIncentivoFiscalISS: Integer = 1;  // Serviço apenas: para indicar se o estado é participante ou não da (Lei do Incentivo Fiscal – ISS), valores: 1 (participante) ou 2 (não participante)
           CodigoIBGE: String = '';         // Serviço apenas: Código do município
           ModalidadeBCICMSST: Integer = 0; // ICMS ST: Modalidade Base de Calculo, 0 – Preço tabelado ou máximo sugerido
                                            //       Substituição Tributária        1 – Lista negativa (valor)
                                            //                                      2 – Lista positiva (valor)
                                            //                                      3 – Lista neutra (valor)
                                            //                                      4 – Margem do valor agregado (%)
                                            //                                      5 – Pauta (valor)
           PercentualMargemICMSST: Double = 0;    // ICMS ST:
           PercentualReducaoBCICMSST: Double = 0; // ICMS ST:
           ValorReducaoBCICMSST: Double = 0;      // ICMS ST:
           AliquotaICMSST: Double = 0;            // ICMS ST:
           ValorICMSST: Double = 0;               // ICMS ST:
           ValorICMSDesonerado: Double = 0;
           MotivoDesoneracaoICMS: Integer = 9;    // 3 – Uso na agropecuária; 9 – Outros; 12 – Órgão de fomento e desenvolvimento agropecuário
           CST_PIS: String = '';
           BaseCalculoPIS: Double = 0;
           AliquotaPIS: Double = 0;
           ValorPIS: Double = 0;
           QuantidadeVendidaPIS: Double = 0;
           ValorAliquotaPIS: Double = 0;
           CST_COFINS: String = '';
           BaseCalculoCOFINS: Double = 0;
           AliquotaCOFINS: Double = 0;
           ValorCOFINS: Double = 0;
           QuantidadeVendidaCOFINS: Double = 0;
           ValorAliquotaCOFINS: Double = 0;
           CEST: String = ''); override;

    Procedure DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo : Double = 0;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0 ) ;  override ;
    Procedure CancelaDescontoAcrescimoItem( NumItem: Integer;
       TipoAcrescimoDesconto: String = 'D' ) ; override;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '') ; override ;
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure NaoFiscalCompleto( CodCNF : String; Valor : Double;
       CodFormaPagto  : String; Obs : AnsiString; IndiceBMP : Integer = 0) ; override ;
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ); override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; override ;
    Procedure EfetuaPagamentoNaoFiscal( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false) ; override ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaNaoFiscal ; override ;

    Procedure LeituraX ; override ;
    Procedure LeituraXSerial( Linhas : TStringList) ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;

    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; override ;
    Procedure CancelaImpressaoCheque ; override ;

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;
    Procedure MudaArredondamento( Arredondar : Boolean ) ; override ;
    Procedure CorrigeEstadoErro(Reducao: Boolean = True) ; override ;
    Procedure ImpactoAgulhas( NivelForca : Integer = 2) ; override ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ); override ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal : Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ; override ;
    Procedure LeituraMFDSerial(DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; override ;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ; override ;
    Function RetornaInfoECF( Registrador: String) : AnsiString; override ;

    Procedure AbreGaveta ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( var AliquotaICMS: String ):
       TACBrECFAliquota; override ;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    function AchaFPGDescricao( Descricao : String;
       BuscaExata : Boolean = False;
       IgnorarCase : Boolean = True;
       IgnorarAcentos : Boolean = False) : TACBrECFFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaRelatoriosGerenciais ; override ;
    procedure LerTotaisRelatoriosGerenciais ; override ;
    Procedure ProgramaRelatorioGerencial( var Descricao: String;
       Posicao : String = '') ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;

    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;

    Procedure EspelhoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; override ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; override ;

    Procedure ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString); override;
    procedure ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD; const NomeArquivo: AnsiString;
      StrInicial, StrFinal: AnsiString); override;

    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD  ) ; override ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       const NomeArquivo : AnsiString;
       Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD;
       TipoContador: TACBrECFTipoContador = tpcCOO ) ; override ;
    function TraduzirTag(const ATag: AnsiString): AnsiString; override;
    function TraduzirTagBloco(const ATag, Conteudo: AnsiString): AnsiString; override;

    procedure PafMF_GerarCAT52(const DataInicial, DataFinal: TDateTime;
      const DirArquivos: String; NumeroSerie: String = ''); override;

 end ;

function BematechTraduzirTag(const ATag: AnsiString): AnsiString;
function BematechTraduzirTagBloco(const ATag, Conteudo: AnsiString;
  AECFClass: TACBrECFClass): AnsiString;

implementation
Uses
   {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
   SysUtils, IniFiles, math,
   {$IFDEF COMPILER6_UP} DateUtils, StrUtils, {$ELSE} ACBrD5,{$ENDIF}
   ACBrConsts, ACBrECF, ACBrECFEscECF, ACBrUtil;

function BematechTraduzirTag(const ATag : AnsiString) : AnsiString ;
const
  C_ON  = #1 ;
  C_OFF = #0 ;

  // <e></e>
  cExpandidoOn   = SO;
  cExpandidoOff  = #20;

  // <n></n>
  cNegritoOn     = ESC + 'E';
  cNegritoOff    = ESC + 'F';

  // <s></s>
  cSublinhadoOn  = ESC + '-' + C_ON;
  cSublinhadoOff = ESC + '-' + C_OFF;

  // <c></c>
  cCondensadoOn  = ESC + SI;
  cCondensadoOff = DC2;

  //<i></i>
  cItalicoOn  = ESC + '4';
  cITalicoOff = ESC + '5';
begin

  if ATag = cTagLigaExpandido then
    Result := cExpandidoOn
  else if ATag = cTagDesligaExpandido then
    Result := cExpandidoOff
  else if ATag = cTagLigaNegrito then
    Result := cNegritoOn
  else if ATag = cTagDesligaNegrito then
    Result := cNegritoOff
  else if ATag = cTagLigaSublinhado then
    Result := cSublinhadoOn
  else if ATag = cTagDesligaSublinhado then
    Result := cSublinhadoOff
  else if ATag = cTagLigaCondensado then
    Result := cCondensadoOn
  else if ATag = cTagDesligaCondensado then
    Result := cCondensadoOff
  else if ATag = cTagLigaItalico then
    Result := cItalicoOn
  else if ATag = cTagDesligaItalico then
    Result := cITalicoOff
  else
     Result := '' ;
end;

function BematechTraduzirTagBloco(const ATag, Conteudo : AnsiString;
  AECFClass: TACBrECFClass) : AnsiString ;
const
  cEAN8     = 'D' ; // <ean8></ean8>
  cEAN13    = 'C' ; // <ean13></ean13>
  cINTER25  = 'F' ; // <inter></inter>
  cCODE39   = 'E' ; // <code39></code39>
  cCODE93   = 'H' ; // <code93></code93>
  cCODE128  = 'I' ; // <code128></code128>
  cUPCA     = 'A' ; // <upca></upca>
  cCODABAR  = 'G' ; // <codabar></codabar>
  cMSI      = #130; // <msi></msi>

  function MontaCodBarras(const ATipo: AnsiString; ACodigo: AnsiString;
    TamFixo: Integer = 0): AnsiString;
  var
    L, A : Integer ;
    M1, M2 : Byte ;
  begin
    with AECFClass.ConfigBarras do
    begin
      L  := IfThen( LarguraLinha = 0, 2, max(min(LarguraLinha,4),2) );
      A  := IfThen( Altura = 0, 50, max(min(Altura,255),1) );
      M1 := Byte(Margem SHR 0);
      M2 := Byte(Margem SHR 8);
    end ;

    ACodigo := Trim( ACodigo );
    if TamFixo > 0 then
       ACodigo := PadLeft( ACodigo, TamFixo, '0') ;

    Result := GS + 'w' + chr( L ) + // Largura
              GS + 'h' + chr( A ) + // Altura
	      GS + 'k' + #132 + char(M1) + char(M2) + // Margem
              GS + 'H' + ifthen( AECFClass.ConfigBarras.MostrarCodigo, #2, #0 ) +
              GS + 'k' + ATipo + chr( Length( ACodigo ) ) + ACodigo;
  end;

  Function AddStartStop( Conteudo: AnsiString ) : AnsiString ;
  begin
    Result := Trim(Conteudo) ;
    if LeftStr(Result,1) <> '*' then
       Result := '*'+Result;
    if RightStr(Result,1) <> '*' then
       Result := Result+'*' ;
  end ;

var
   Is010000, IsEscECF : Boolean ;

begin
  // MP4000 ver 01.00.00 tem sérios problemas quando tenta imprimir CODE39 ou CODEBAR
  IsEscECF := (AECFClass is TACBrECFEscECF);
  Is010000 := (not IsEscECF) and (StrToIntDef( AECFClass.NumVersao,0 ) <= 10000) ;

  if ATag = cTagBarraEAN8 then
    Result := MontaCodBarras(cEAN8, Conteudo, 7)
  else if ATag = cTagBarraEAN13 then
    Result := MontaCodBarras(cEAN13, Conteudo, 12)
  else if ATag = cTagBarraInter then
    Result := MontaCodBarras(cINTER25, Conteudo)
  else if ATag = cTagBarraCode39 then
  begin
    if Is010000 then
      Result := ''
    else if IsEscECF then
      Result := MontaCodBarras(cCODE39, Conteudo )
    else
      Result := MontaCodBarras(cCODE39, AddStartStop(Conteudo) );
  end
  else if ATag = cTagBarraCode93 then
    Result := MontaCodBarras(cCODE93, Conteudo)
  else if ATag = cTagBarraCode128 then
    Result := MontaCodBarras(cCODE128, Conteudo)
  else if ATag = cTagBarraUPCA then
    Result := MontaCodBarras(cUPCA, Conteudo, 11)
  else if ATag = cTagBarraCodaBar then
  begin
    if Is010000 then
      Result := ''
    else if IsEscECF then
      Result := MontaCodBarras(cCODABAR, Conteudo )
    else
      Result := MontaCodBarras(cCODABAR, AddStartStop(Conteudo) );
  end
  else if ATag = cTagBarraMSI then
    Result := MontaCodBarras(cMSI, Conteudo)
  else
     Result := Conteudo;
end;

{ ----------------------------- TACBrECFBematech ------------------------------ }

constructor TACBrECFBematech.create( AOwner : TComponent ) ;
begin
  inherited create( AOwner ) ;

  fpDevice.HandShake := hsRTS_CTS ;
  { Variaveis internas dessa classe }
  fsST1       := 0 ;
  fsST2       := 0 ;
  fsST3       := 0 ;
  fsACK       := 0 ;
  fsBytesResp := 0 ;
  fsTotalPago := 0 ;
  fsPAF       := '' ;
  fsNumVersao := '' ;
  fsNumECF    := '' ;
  fsNumLoja   := '' ;
  fsNumCRO    := '' ;
  fsSubModeloECF  := '' ;
  fsArredonda := ' ';
  fsTotalizadoresParciais := '' ;
  fsFalhasFimImpressao    := 0 ;
  fsNumCOOInicial := '' ;
  fsNFCodCNF := '' ;
  fsNFCodFPG := '' ;
  fsNFValor  := 0  ;
  fsProp     := '' ;

  fsModelosCheque := TACBrCHQModelos.create( true );

  fpModeloStr := 'Bematech' ;
  fpRFDID     := 'BE' ;
  fpPaginaDeCodigo := 850 ;

  fpIdentificaConsumidorRodape := True ;
end;

destructor TACBrECFBematech.Destroy;
begin
  fsModelosCheque.Free ;

  inherited Destroy ;
end;

procedure TACBrECFBematech.Ativar;
var
  wRetentar : Boolean ;
begin
  if not fpDevice.IsSerialPort  then
     raise EACBrECFERRO.Create(ACBrStr('A impressora: '+fpModeloStr+' requer'+sLineBreak+
                                'Porta Serial:  (COM1, COM2, COM3, ...)'));

  inherited Ativar ; { Abre porta serial }

  fsNumVersao := '' ;
  fsNumECF    := '' ;
  fsNumLoja   := '' ;
  fsNumCRO    := '' ;
  fsSubModeloECF := '' ;
  fpMFD       := false ;
  fpTermica   := false ;
  fsArredonda := ' ';
  fsTotalizadoresParciais := '' ;
  fsNumCOOInicial := '' ;
  fsNFCodCNF  := '' ;
  fsNFCodFPG  := '' ;
  fsNFValor   := 0 ;
  fs25MFD     := false ;
  fsProp      := '';

  try
     { Testando a comunicaçao com a porta }
     try
        fs25MFD   := True;     // Tenta conectar com protocolo Novo (28)
        wRetentar := Retentar ;
        try
           Retentar := False;
           EnviaComando( #19 ) ;    { Pede Status }
        finally
           Retentar := wRetentar;
        end ;
     except
        fs25MFD := False;      // Protocolo Novo não foi entendido, usando antigo (27)
        EnviaComando( #19 ) ;       { Pede Status }
     end ;

     if not ACKValido(fsACK) then
        raise EACBrECFNaoInicializado.Create( ACBrStr(
                 'Erro inicializando a impressora '+fpModeloStr ));

     NumVersao ;   { Inicializa fpMFD, fsNumVersao e fpTermica }
     GetProp;      { Inicializa o Numero do Proprietário }

     { Verificando se é MP40. Se for MP40 tem apenas 40 colunas e não 48 colunas
       Se os 4 primeiros dígitos do Numero de série forem 4708, corresponde
       ao modelo MP-20. Se os 4 dígitos forem 5708, corresponde ao modelo MP-40 }
     if copy(NumSerie,1,4) = '5708' then
        fpColunas := 40 ;
  except
     Desativar ;
     raise ;
  end ;
end;


function TACBrECFBematech.EnviaComando_ECF(cmd : AnsiString) : AnsiString ;
Var
  ErroMsg : String ;
  B : Byte ;
  PediuStatus : Boolean ;
  FalhasACK   : Integer;
  nSTL, nSTH  : Integer;

  procedure AnalisaACK ;
  begin
     if fsACK = 0 then
        raise EACBrECFSemResposta.create( ACBrStr(
                 'Impressora '+fpModeloStr+' não responde (ACK = 0)'))
     else if fsACK = 21 then    { retorno em caracter 21d=15h=NAK }
        raise EACBrECFSemResposta.create( ACBrStr(
              'Impressora '+fpModeloStr+' não reconheceu o Comando'+
              sLineBreak+' (ACK = 21)'))
     else if not ACKValido(fsACK) then
        raise EACBrECFSemResposta.create( ACBrStr(
              'Erro. Resposta da Impressora '+fpModeloStr+' inválida'+
              sLineBreak+' (ACK = '+IntToStr(fsACK)+')')) ;
  end ;

begin
  fsACK   := 0  ;
  fsST1   := 0  ;
  fsST2   := 0  ;
  fsST3   := 0  ;
  nSTL    := 0  ;
  nSTH    := 0  ;
  Result  := '' ;
  ErroMsg := '' ;
  fpComandoEnviado     := '' ;
  fpRespostaComando    := '' ;
  fsFalhasFimImpressao := 0  ;

  PediuStatus := ( cmd = #19 ) ; { quando pede Status nao deve disparar
                                   exceçao com erros "Pouco Papel" ou "Cupom Aberto" }

  { Codificando CMD de acordo com o protocolo da Bematech }
  cmd := PreparaCmd( cmd ) ;

  try
     fpDevice.Serial.DeadlockTimeout := 2000 ; { Timeout p/ Envio }
     FalhasACK := 0 ;

     while not ACKValido(fsACK) do
     begin
        fsACK := 0 ;
        fpDevice.Serial.Purge ;                { Limpa a Porta }

        if not TransmiteComando( cmd ) then
           continue ;

        try
           { espera ACK chegar na Porta por TimeOut segundos }
           try
              fsACK := fpDevice.LeByte( TimeOut * 1000 ) ;
           except
           end ;

           GravaLog('   '+FormatDateTime('hh:nn:ss:zzz',now)+' RX <- ACK = '+IntToStr(ACK)+' Falha: '+IntToStr(FalhasACK) ) ;

           AnalisaACK;
        except
           on E : EACBrECFSemResposta do
           begin
              fpDevice.Serial.Purge ;

              Inc( FalhasACK ) ;

              if FalhasACK < 3 then
                 Sleep(100)
              else
                 if not DoOnMsgRetentar( E.Message +sLineBreak+sLineBreak+
                    'Se o problema persistir, verifique os cabos, ou'+sLineBreak+
                    'experimente desligar a impressora durante 5 seg,'+sLineBreak+
                    'liga-la novamente, e repetir a operação...'
                    , 'LerACK') then
                    raise ;
           end ;
           else
              raise ;
        end ;
     end ;

     fpComandoEnviado := cmd ;

     { Chama Rotina da Classe mãe TACBrClass para ler Resposta. Se houver
        falha na leitura LeResposta dispara Exceçao }
     LeResposta ;

     { Separando o Retorno }
     try
        if BytesResp >= 0 then
         begin
           fsST1 := ord( fpRespostaComando[ BytesResp + 1 ] ) ;
           fsST2 := ord( fpRespostaComando[ BytesResp + 2 ] ) ;

           if fs25MFD then
           begin
              try
                 nSTL := ord( fpRespostaComando[ BytesResp + 3 ] );
                 nSTH := ord( fpRespostaComando[ BytesResp + 4 ] );
              except
              end;
           end ;

           Result := copy(fpRespostaComando, 1, BytesResp) ;
         end
        else  { Quando BytesResp < 0 espera por ETX no final }
         begin
           fsST1 := ord( fpRespostaComando[ 1 ] ) ;
           fsST2 := ord( fpRespostaComando[ 2 ] ) ;

           if fs25MFD then
            begin
               try
                  nSTL := ord( fpRespostaComando[ 3 ] );
                  nSTH := ord( fpRespostaComando[ 4 ] );
               except
               end;

               Result := copy(fpRespostaComando, 5, Length(fpRespostaComando)-5 ) ;
            end
           else
              Result := copy(fpRespostaComando, 3, Length(fpRespostaComando)-3 ) ;
         end ;
     except
     end ;

     if fs25MFD then
        fsST3 := (nSTH shl 8) + nSTL;

     { Verifica se possui erro "Pouco Papel" }
     if TestBit( ST1, 6 ) then
        DoOnMsgPoucoPapel ;

     ErroMsg := '' ;
     // Alguns erros estendidos não impedem de continuar a 'conversa' com o ECF
     // por isso só busca o erro estendido se realmente tiver algum erro grave!
     if fs25MFD then
      begin
        if (fsST3 > 0) and (fsST3 <= High(ErrosST3)) and
           (fsST1+fsST2 <> 0) then
           ErroMsg := ErrosST3[fsST3] + sLineBreak
      end
     else
      begin
        { Verificando por erros em ST1 e ST2 }
        For B := 0 to 7 do
        begin
           if (B <> 6) and TestBit( ST1, B) then
              if (not PediuStatus) or (B <> 1) then
                 ErroMsg := ErroMsg + ErrosST1[ B ] + sLineBreak ;

           if TestBit( ST2, B) then
              ErroMsg := ErroMsg + ErrosST2[ B ] + sLineBreak ;
        end ;
      end ;

     if ErroMsg <> '' then
      begin
        ErroMsg := 'Erro retornado pela Impressora: ' + fpModeloStr + sLineBreak+sLineBreak+
                   ErroMsg ;

        if TestBit(fsST1, 7) or (fsST3 = 11) then
           DoOnErrorSemPapel
        else
           raise EACBrECFSemResposta.create(ACBrStr(ErroMsg)) ;
      end
     else
        Sleep( IntervaloAposComando ) ;  { Pequena pausa entre comandos }
  finally
     BytesResp := 0;
  end ;
end;


function TACBrECFBematech.VerificaFimLeitura(var Retorno: AnsiString;
   var TempoLimite: TDateTime) : Boolean ;
Var
  LenRet, LenST : Integer ;
  Buffer: AnsiString;
begin
  LenRet := Length(Retorno) ;

  { Lê até atingir todos os Bytes esperados (BytesResp) e ECF entra EmLinha}
  { BytesResp é necessário, pois a Bematech nao usa um Sufixo padrão no fim
    da resposta da Impressora. }
  if BytesResp >= 0 then
   begin
     { Somei + 2 em BytesResp pois a Bematech tambem envia ST1 e ST2 no final
       de cada comando
      Se for MFD ou MP25 envia ainda STL e STH bytes do status estendido }
     if fs25MFD then
        LenST := 4
     else
        LenST := 2 ;

     Result := (LenRet >= (BytesResp + LenST) )
   end
  else
     if (Length(Retorno) >= 2) and (copy(Retorno,1,2) <> #0+#0) then  // Retornou Erro, não virá ETX
        Result := True
     else
      begin
        Result := (pos(ETX, RightStr(Retorno,6)) > 0) ;
        if Result then
        begin
           try
              Buffer := fpDevice.LeString(1000);  // Aguarda por 1 seg sem dados
              if Buffer <> '' then
              begin
                 Retorno := Retorno + Buffer;
                 TempoLimite := IncSecond(now, TimeOut);
                 Result := False;
              end;
           except
           end;
        end;
      end;

  { Nota sobre o VerificaFimLeitura: A Bematech responde muito antes da
    Impressao terminar, o que pode causar problemas com comandos enviados logo
    após impressoes demoradas como a Leitura X (por exemplo). Para esses casos,
    é necessário ativar a propriedade "AguardaImpressao := True" }
end;

function TACBrECFBematech.VerificaFimImpressao(var TempoLimite: TDateTime): Boolean;
Var Cmd , RetCmd : AnsiString ;
    wACK : Byte ;
begin
  { Essa função só é chamada se AguardaImpressao = True,
    Como essa função é executada dentro da "LeResposta", que por sua vez foi
    chamada por "EnviaComando", não podemos usar o método "EnviaComando" (ou
    teriamos uma chamada recursiva infinita), por isso o Loop abaixo envia o
    comando #19 diretamente para a Serial, e aguarda por 1 segundo a resposta...
     Se a Bematech conseguir responder, significa que a Impressão Terminou }
  wACK   := 0 ;
  Result := false ;
  if not EmLinha() then
   begin
     Sleep(100) ;
     GravaLog('   '+FormatDateTime('hh:nn:ss:zzz',now)+' VerificaFimImpressao: ECF fora de linha' ) ;
   end
  else
   begin
     RetCmd := '' ;
     Cmd    := PreparaCmd( #19 ) ;           // Pede Status //

     try
        GravaLog('   '+FormatDateTime('hh:nn:ss:zzz',now)+' VerificaFimImpressao: Pedindo o Status (19)' ) ;

        fpDevice.Serial.Purge ;           // Limpa buffer de Entrada e Saida //
        fpDevice.EnviaString( Cmd );         // Envia comando //

        // espera ACK chegar na Porta por 1,5s //
        wACK := fpDevice.LeByte( 1500 ) ;

        if ACKValido(wACK) then   // ECF Respondeu corretamente, portanto está trabalhando //
         begin
           GravaLog('   '+FormatDateTime('hh:nn:ss:zzz',now)+' VerificaFimImpressao: ACK = '+IntToStr(wACK)+', OK... Aguardando ST1 e ST2' ) ;
           TempoLimite := IncSecond(now, TimeOut);
           fsFalhasFimImpressao := 0 ;

           // Aguarda ST1 e ST2 por mais 2 segundos //
           RetCmd := fpDevice.LeString( 2000, 2 ) ;
           Result := (Length( RetCmd ) >= 2) ;
         end
        else
           raise EACBrECFErro.Create( 'ACK não válido' );
     except
       On E: Exception do
       begin
          if (not ACKValido(wACK)) and (BytesResp < 0) then  { Incrementa Falhas apenas na leitura de Relatorios pela Serial }
             Inc( fsFalhasFimImpressao ) ;

          GravaLog('   '+FormatDateTime('hh:nn:ss:zzz',now)+' VerificaFimImpressao: ACK = '+IntToStr(wACK)+
                   ' - Falhas = '+IntToStr(fsFalhasFimImpressao)+
                   ' - Erro: ' +E.ClassName + ' ' + E.Message,True) ;
       end ;
     end ;
   end ;

   if fsFalhasFimImpressao > 10 then
      raise EACBrECFSemResposta.create( ACBrStr(
             'VerificaFimImpressao: Impressora '+fpModeloStr+' não está em linha')) ;

end;

function TACBrECFBematech.PreparaCmd( cmd : AnsiString ) : AnsiString ;  // Adaptada do manual da Bematech //
Var A, iSoma, LenCmd : Integer ;
    NBL, NBH, CSL, CSH : AnsiChar ;
begin

  result := '' ;
  if cmd = '' then exit ;

  if not CharInSet(cmd[1], [ESC, FS]) then
  begin
    if fs25MFD then
       cmd := FS + cmd
    else
       cmd := ESC + cmd ;   { Prefixo ESC }
  end;

  { Calculando a Soma dos caracteres ASC }
  iSoma := 0 ;
  For A := 1 to Length(cmd) do
     iSoma := iSoma + ord( cmd[A] ) ;

  { Calculando os dígitos }
  LenCmd := Length( CMD ) + 2  ;    { + 2 = CSL + CSH }
  NBL    := AnsiChar( chr( LenCmd mod 256 ) ) ;
  NBH    := AnsiChar( chr( Trunc( LenCmd / 256 ) ) ) ;
  CSL    := AnsiChar( chr( iSoma mod 256 ) ) ;
  CSH    := AnsiChar( chr( Trunc(iSoma / 256 ) ) ) ;

  Result := STX + NBL + NBH + CMD + CSL + CSH ;
end ;


function TACBrECFBematech.GetDataHora: TDateTime;
Var
  RetCmd : AnsiString ;
begin
  RetCmd := RetornaInfoECF( '23' ) ;
  Result := StringToDateTime( copy(RetCmd, 1,2) + DateSeparator +
                              copy(RetCmd, 3,2) + DateSeparator +
                              copy(RetCmd, 5,2) + ' ' +
                              copy(RetCmd, 7,2) + TimeSeparator +
                              copy(RetCmd, 9,2) + TimeSeparator +
                              copy(RetCmd,11,2),
                              'dd/mm/yy hh:nn:ss' ) ;
end;

function TACBrECFBematech.GetNumCupom: String;
begin
  BytesResp := 3 ;
  Result := Trim( BcdToAsc( EnviaComando( #30 ) ) ) ;
end;

function TACBrECFBematech.GetNumCRO: String;
begin
  if fsNumCRO = '' then
     fsNumCRO := RetornaInfoECF( '10' ) ;

  Result := fsNumCRO ;
end;

function TACBrECFBematech.GetNumCCF: String;
begin
  Result := '' ;
  if fpMFD then
  begin
     try
        { Comando disponivel apenas a partir da MP2100 }
        Result := RetornaInfoECF( '55' ) ;
     except
     end ;
  end ;
end;

function TACBrECFBematech.GetNumLoja: String;
begin
  if fsNumLoja = '' then
     fsNumLoja := RetornaInfoECF( '15' ) ;

  Result := fsNumLoja ;
end;

function TACBrECFBematech.GetNumECF: String;
begin
  if fsNumECF = '' then
     fsNumECF  := RetornaInfoECF( '14' ) ;

  Result := fsNumECF ;
end;

function TACBrECFBematech.GetNumSerie: String;
var
  wRetentar, Falhou40 : Boolean ;
begin
  Result    := '' ;
  Falhou40  := True;
  wRetentar := Retentar ;

  if fs25MFD then
  begin
     Retentar := false ;
     try
        try
           Result   := Trim( RetornaInfoECF( '40' ) ) ;
           Falhou40 := False;
        except
        end ;
     finally
        Retentar := wRetentar ;
     end ;
  end ;

  if Falhou40 then
     Result := Trim( RetornaInfoECF( '00' ) ) ;
end;

function TACBrECFBematech.GetNumSerieMFD: String;
begin
  Result := '' ;
  if fpMFD then
     Result := RetornaInfoECF( '58' ) ;
end;

function TACBrECFBematech.GetNumVersao: String ;
var wRetentar : Boolean ;
//    wTimeOut  : Integer ;
    RetCmd    : AnsiString ;
begin
  if fsNumVersao = '' then
  begin
     try
        wRetentar := Retentar ;
//        wTimeOut  := TimeOut ;
        try
           Retentar    := false ;
//           TimeOut     := 1 ;
           fsNumVersao := Trim( RetornaInfoECF( '41' )) ;
           fs25MFD     := True ;
           try
              RetCmd    := Trim( RetornaInfoECF( '60' )) ;
              fsSubModeloECF := copy(RetCmd,16,20) ;
              fpTermica := (Pos('TH ',RetCmd) > 0) or (Pos('VIRTUAL ',RetCmd) > 0) ;
              fpMFD     := fpTermica ;
           except
           end ;
        finally
           Retentar := wRetentar ;
//           TimeOut  := wTimeOut ;
        end ;
     except
        fpMFD       := False ;
        fsNumVersao := Trim( RetornaInfoECF( '01' ))  ;
     end ;
  end ;

  Result := fsNumVersao ;
end;

function TACBrECFBematech.GetTotalPago: Double;
begin
   Result := RoundTo( StrToFloatDef( RetornaInfoECF( '22' ), -1 ) / 100, -2) ;
   if Result < 0 then
      Result := fsTotalPago ;
end;

function TACBrECFBematech.GetSubTotal: Double;
Var
  RetCmd : AnsiString ;
  B2 : Integer ;
  ENaoFiscal : Boolean ;
begin
  ENaoFiscal := False;
  if fpMFD then
  begin
     RetCmd     := RetornaInfoECF( '65' ) ;
     B2         := ord( RetCmd[1] )  ;
     ENaoFiscal := TestBit( B2 ,0) ;
  end ;

  if not ENaoFiscal then
   begin
     BytesResp := 7 ;
     RetCmd    := BcdToAsc( EnviaComando( #29 ) ) ;
   end
  else
     RetCmd := RetornaInfoECF('66') ;

  Result := StrToFloatDef(RetCmd, 0) / 100 ;
end;

{  Ordem de Retorno do Estado da Impressora
   estNaoInicializada - Não Inicializada (Nova)
   estDesconhecido    - Desconhecido
   estPagamento       - Cupom Venda Aberto em Pagamento
   estVenda           - Cupom Venda Aberto em Itens
   estNaoFiscal       - Cupom Não Fiscal Aberto
   estRelatorio       - Cupom Vinculado Aberto | Relatório Gerencial Aberto
   estBloqueada       - Impressora Bloqueada para venda
   estRequerZ         - Requer Emissão da Redução da Z
   estRequerX         - Requer Leitura X
   estLivre           - Livre para vender
}
function TACBrECFBematech.GetEstado: TACBrECFEstado;
Var RetCmd : AnsiString ;
    DataMov, DataHora, Data: TDateTime ;
    B1, B2 : Byte ;
begin
  fpEstado := estNaoInicializada ;
  if (not fpAtivo) then
  begin
    Result := fpEstado ;
    Exit ;
  end;

  try
    fpEstado := estDesconhecido ;
    RetCmd   := RetornaInfoECF( '17' ) ;

    try B1 := ord( RetCmd[1] ) except B1 := 0 end ;

    if TestBit( B1 ,1) then
      fpEstado := estPagamento
    else if TestBit( B1 ,0) then
      fpEstado := estVenda
    else if TestBit( B1 ,3) then
      fpEstado := estBloqueada
    else
     begin
       if fpMFD and fpTermica then    { Bematech Matricial, nao possui Flag para }
       begin                          { inidicar se está Imprimindo Relatório }
         try                          { (Cupom Fiscal Vinculado ou Relatorio Gerencial) }
            RetCmd := RetornaInfoECF( '65' ) ;
            B2     := ord( RetCmd[1] )  ;

            if TestBit( B2 ,0) then
              fpEstado := estNaoFiscal
            else if TestBit( B2 ,1) or TestBit( B2 ,2) then
              fpEstado := estRelatorio
         except
         end ;
       end ;
     end ;

    if fpEstado = estDesconhecido then
    begin
       fpEstado := estLivre ;
       DataMov := Self.GetDataMovimento;

       if (DataMov > 0) then
       begin
          DataHora := Self.GetDataHora;
          Data := DateOf(DataHora);
          if (DataMov < Data) and ((HoursBetween(Data, DataHora) > 2)) then
            fpEstado := estRequerZ ;
       end;
//     else
//     begin
//       fpEstado :=  estRequerX ;
//       { OBS.: comentado pois a Leitura X na Bematech não abre o Movimento,
//       apenas a abertura de cupom, inicializa a DataMov }
//     end;
    end ;
  finally
    Result := fpEstado ;
  end ;
end;

function TACBrECFBematech.GetGavetaAberta: Boolean;
Var B : Byte ;
    Resp : AnsiString ;
begin
  BytesResp := 1 ;
  Resp := EnviaComando( #23 ) ;
  B := ord( Resp[1] ) ;
  Result := (B <> 0) ;
end;

function TACBrECFBematech.GetPoucoPapel: Boolean;
begin
  Result := TestBit(fsST1, 6) ;
end;

function TACBrECFBematech.GetHorarioVerao: Boolean;
Var RetCmd : AnsiString ;
    B : Byte ;
begin
   RetCmd := RetornaInfoECF( '17' ) ;
   try B := ord( RetCmd[1] ) except B := 0 end ;

   Result := TestBit( B ,2)
end;

function TACBrECFBematech.GetArredonda: Boolean;
Var RetCmd : AnsiString ;
    B : Byte ;
begin
  if fsArredonda = ' ' then
  begin
     RetCmd := RetornaInfoECF( '28' ) ;
     try B := ord( RetCmd[1] ) except B := 0 end ;

     if (B <> 0) then
        fsArredonda := 'S'
     else
        fsArredonda := 'N' ;
  end ;

  Result := (fsArredonda = 'S') ;
end;

procedure TACBrECFBematech.LeituraX ;
 Var Espera : Integer ;
begin
  Espera := 40 ;
  if fpMFD then
     Espera := 20 ;

  BytesResp := 0 ;
  AguardaImpressao := True ;
  EnviaComando( #06, Espera ) ;
end;

procedure TACBrECFBematech.LeituraXSerial(Linhas: TStringList);
begin
  BytesResp := -1 ; { espera por ETX }
  Linhas.Clear ;
  Linhas.Text := EnviaComando( #69, 10 ) ;
end;

procedure TACBrECFBematech.AbreGaveta ;
begin
  BytesResp := 0 ;
  EnviaComando( #22 + #100 ) ;
  sleep(100) ;
end;

procedure TACBrECFBematech.ReducaoZ(DataHora : TDateTime) ;
var
  DataStr: AnsiString;
  Espera : Integer ;
begin
  Espera := 40 ;
  if fpMFD then
     Espera := 30 ;

  BytesResp := 0 ;

  DataStr := '' ;
  if DataHora <> 0 then
     DataStr := FormatDateTime('ddmmyyhhnnss',DataHora) ;

  AguardaImpressao := True ;
  EnviaComando( #05 + DataStr, Espera );
end;

procedure TACBrECFBematech.MudaHorarioVerao ;
begin
  BytesResp := 0 ;
  EnviaComando( #18 ) ;
end;

procedure TACBrECFBematech.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  if EHorarioVerao <> HorarioVerao then
     MudaHorarioVerao ;
end;
 
procedure TACBrECFBematech.MudaArredondamento(Arredondar: Boolean);
var
  Valor: Byte;
begin
  BytesResp := 0 ;

  if ( Arredondar ) then
    Valor := 1
  else
    Valor := 0;

  EnviaComando( #39 + chr( Valor ) ) ;
end;

procedure TACBrECFBematech.AbreBilhetePassagem(Origem: string; Destino: string;
  Linha: string; Agencia: string; DataHora: TDateTime; Poltrona: string;
  Plataforma: string; Tipo: TACBrECFTipoBilhete; UFDestino: string;
  PassageiroRG: string; PassageiroNome: string; PassageiroEnd: string);
var
  StrComando: String;

  function GetTipoStr(ATipo: TACBrECFTipoBilhete): Char;
  begin
    case ATipo of
      tbRodIntermun: Result := #48; // 0x30 Rodoviário Intermunicipal;
      tbFerIntermun: Result := #49; // 0x31 Ferroviário Intermunicipal;
      tbAquIntermun: Result := #50; // 0x32 Aquaviário Intermunicipal;
      tbRodInterest: Result := #51; // 0x33 Rodoviário Interestadual;
      tbFerInterest: Result := #52; // 0x34 Ferroviário Interestadual;
      tbAquInterest: Result := #53; // 0x35 Aquaviário Interestadual;
      tbRodInternac: Result := #54; // 0x36 Rodoviário Internacional;
      tbFerInternac: Result := #55; // 0x37 Ferroviário Internacional;
      tbAquInternac: Result := #56; // 0x38 Aquaviário Internacional;
    else
      raise EACBrECFErro.Create('Tipo de Bilhete de passagem desconhecido!');
    end;
  end;

begin
  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }
  BytesResp := 0 ;
  AguardaImpressao := True ;

  StrComando := '011' +
    PadRight(Origem, 40) +
    PadRight(Destino, 40) +
    PadRight(Linha, 40) +
    PadRight('', 40) + // prefixo
    PadRight('', 40) + // agente
    PadRight(Agencia, 40) +
    FormatDateTime('ddmmyy', DataHora) +
    FormatDateTime('hhmmss', DataHora) +
    PadRight(Poltrona, 2) +
    PadRight(Plataforma, 3) +
    GetTipoStr( Tipo ) +
    PadRight(UFDestino, 2) +
    PadRight(PassageiroRG, 29) +
    PadRight(PassageiroNome, 30) +
    PadRight(PassageiroEnd, 80) ;

  EnviaComando( #37 + StrComando, 10) ;

  Consumidor.Enviado := False ;
  fsTotalPago := 0 ;
end;

procedure TACBrECFBematech.AbreCupom  ;
Var StrConsumidor : String ;
begin
  StrConsumidor := '' ;
  if Trim(Consumidor.Documento) <> '' then    { Tem Docto ? }
  begin
     StrConsumidor := PadRight(Consumidor.Documento ,29) ;

     if fs25MFD then
     begin
        if Trim(Consumidor.Nome) <> '' then      { Tem Nome ? }
        begin
           StrConsumidor := StrConsumidor + PadRight(Consumidor.Nome ,30) ;

           if Trim(Consumidor.Endereco) <> '' then  { Tem endereço ? }
              StrConsumidor := StrConsumidor + PadRight(Consumidor.Endereco ,80) ;
        end ;
     end ;
  end ;

  fpUltimaMsgPoucoPapel := 0 ;  { Zera tempo pra msg de pouco papel }
  BytesResp := 0 ;
  AguardaImpressao := True ;
  EnviaComando( #00 + StrConsumidor, 10) ;

  Consumidor.Enviado := ( StrConsumidor <> '' ) ;
  fsTotalPago := 0 ;
end;

procedure TACBrECFBematech.CancelaCupom(NumCOOCancelar: Integer);
 Var RetCmd : AnsiString ;
     B      : Byte ;
     TemRel : Boolean ;
begin
  RetCmd := RetornaInfoECF( '17' ) ;
  try B := ord( RetCmd[1] ) except B := 0 end ;

  if not TestBit(B, 5) then         { Não Permite cancelar cupom fiscal ?? }
  begin
     if not fs25MFD then            { É MP20 ?? }
      begin
        if TestBit( B ,0) then      { Abriu Cupom ?? }
        begin
           { Deve Vender 1 Item para conseguir cancelar }
           VendeItem('00000','CUPOM SERA CANCELADO','NN',1,0.01,0,'') ;
        end ;
      end
     else                           { É MP25 ou MFD ? }
      begin
        { Vamos verificar se o último documento é Vinculado (CDC) }
        try
           RetCmd := RetornaInfoECF( '65' ) ;
           B      := ord( RetCmd[1] )
        except
           B := 0
        end ;

        if TestBit(B, 6) then             // Pode cancelar CDC ?
        begin
           try
              AguardaImpressao := True ;
              BytesResp := 0 ;
              EnviaComando( #102 , 5) ;   // Cancelando o CDC
           except
           end ;
        end ;
     end ;
  end ;

  BytesResp := 0 ;
  AguardaImpressao := True ;
  EnviaComando( #14 , 15) ;
  fsTotalPago := 0 ;
  fsTotalizadoresParciais := '' ;

  if fpMFD then
     TemRel := (Estado = estRelatorio)  // MFD sinaliza estado Relatorio //
  else
     TemRel := True ;                   // Nao MFD não há flag para sinalizar Relatorio Aberto

  if TemRel then
  begin
     try
       FechaRelatorio ;   { Fecha relatorio se ficou algum aberto (só por garantia)}
     except   // Exceçao silenciosa, pois a Impressora pode nao estar em Estado
     end ;    // de Relatorio.
  end ;
end;

procedure TACBrECFBematech.CancelaItemVendido(NumItem: Integer);
begin
  BytesResp := 0 ;
  EnviaComando( #31 + IntToStrZero(NumItem ,4) ) ;
end;

procedure TACBrECFBematech.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
begin
  Observacao := copy(Observacao,1,80) ;
  BytesResp  := 0 ;
  EnviaComando( #72 + CodFormaPagto +
                IntToStrZero( Round(Valor * 100) ,14) +
                Observacao ) ;
  fsTotalPago := fsTotalPago + RoundTo(Valor,-2) ;
end;

procedure TACBrECFBematech.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
Var
  StrConsumidor : String ;
begin
  StrConsumidor := '';
  if (not Consumidor.Enviado) and (Consumidor.Documento <> '') then
  begin
     StrConsumidor := 'CNPJ/CPF: '+Consumidor.Documento + LF;

     if Consumidor.Nome <> '' then
     begin
        StrConsumidor := StrConsumidor + 'Nome: '+Consumidor.Nome + LF;

        if Consumidor.Endereco <> '' then
           StrConsumidor := StrConsumidor + 'Endereco: '+Consumidor.Endereco + LF;
    end;
  end;

  Observacao := StrConsumidor + Observacao;

  if Operador <> '' then
  begin
     if Observacao <> '' then
       Observacao := Observacao + #10;

     Observacao := Observacao + 'Operador: '+Operador;
  end;

  if Copy( Observacao, length( Observacao ), 1) <> #10 then
     Observacao := Observacao + #10 ;

  Observacao := copy(Observacao,1, 492) ; { Limite da Bematech }
  BytesResp  := 0 ;
  AguardaImpressao := True ;
  EnviaComando( #34 + Observacao, 15 ) ;
  fsTotalPago := 0 ;
end;

procedure TACBrECFBematech.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString);
Var A_D : Char ;
begin
  if DescontoAcrescimo < 0 then
     A_D := 'd'
  else
     A_D := 'a' ;

  DescontoAcrescimo := abs(DescontoAcrescimo) ;

  { Inicia fechamento com formas de Pagamento }
  BytesResp  := 0 ;
  EnviaComando( #32 + A_D +
                IntToStrZero( Round( DescontoAcrescimo * 100),14) );
  fsTotalPago := 0 ;
  fsTotalizadoresParciais := '';
end;

procedure TACBrECFBematech.VendeItem(Codigo, Descricao : String ;
  AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
  ValorDescontoAcrescimo : Double ; Unidade : String ;
  TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
  CodDepartamento : Integer) ;
Var
  QtdStr, ValorStr, AcrescimoStr, DescontoStr : String ;
  CMD : Byte ;
begin
  if Qtd > 9999 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 9999.') );

  Descricao := trim(Descricao) ;
  Unidade   := PadRight(Unidade,2) ;

  if fpMFD and fpArredondaItemMFD then
   begin
     BytesResp   := 0 ;
     Codigo      := PadRight(Codigo,14) ;
     QtdStr      := IntToStrZero( Round( Qtd * 1000), 7) ;
     ValorStr    := IntToStrZero( Round( ValorUnitario * 1000), 8) ;
     AcrescimoStr:= StringOfChar('0',4) + #0;
     DescontoStr := AcrescimoStr;

     try
        if ValorDescontoAcrescimo <> 0 then
        begin
             if DescontoAcrescimo = 'A' then
                AcrescimoStr := IntToStrZero( Round(ValorDescontoAcrescimo * 100),
                     ifthen(TipoDescontoAcrescimo = '%', 4, 8) ) + #0
             else
                DescontoStr := IntToStrZero( Round(ValorDescontoAcrescimo * 100),
                     ifthen(TipoDescontoAcrescimo = '%', 4, 8) ) + #0
        end ;

        EnviaComando(#62 + #73 +
                           Codigo + AliquotaECF + Unidade + QtdStr + ValorStr +
                           DescontoStr + AcrescimoStr + Descricao + #0 );
     except
       On E : Exception do
       begin
          if TestBit(ST1,2) then  // Comando inexistente ?
           begin
             fpArredondaItemMFD := False; // Desative o ArredondaItemMFD;

             // Chamada recursiva do método para usar comando tradicional //
             VendeItem( Codigo, Descricao, AliquotaECF, Qtd, ValorUnitario,
                        ValorDescontoAcrescimo, Unidade, TipoDescontoAcrescimo,
                        DescontoAcrescimo );
             exit ;
           end
          else
             raise ;
       end ;
     end ;
   end
  else if fs25MFD or
     ((DescontoAcrescimo <> 'D') and (ValorDescontoAcrescimo > 0)) then   // Tem acrescimo ?
   begin
     fpArredondaItemMFD := False;
     BytesResp   := 0 ;
     Codigo      := Trim(Codigo) ;
     ValorStr    := IntToStrZero( Round( ValorUnitario * 1000), 9) ;
     QtdStr      := IntToStrZero( Round( Qtd * 1000), 7) ;
     AcrescimoStr:= StringOfChar('0',10) ;
     DescontoStr := AcrescimoStr;

     if TipoDescontoAcrescimo = '%' then
      begin
        if Arredonda then
           DescontoStr := IntToStrZero( Round( RoundABNT(ValorUnitario*Qtd,-2) *
                                        ValorDescontoAcrescimo), 10 )
        else
           DescontoStr := IntToStrZero( TruncFix( RoundTo(ValorUnitario*Qtd,-2) *
                                        ValorDescontoAcrescimo), 10 )
      end
     else
        DescontoStr := IntToStrZero(Round(ValorDescontoAcrescimo*100),10);

     if DescontoAcrescimo <> 'D' then // É acrescimo ?
     begin
        AcrescimoStr := DescontoStr ;
        DescontoStr  := StringOfChar('0',10) ;
     end ;

     EnviaComando(#63 + AliquotaECF + ValorStr + QtdStr + DescontoStr +
                        AcrescimoStr + '01' + StringofChar('0',20) + Unidade +
                        Codigo + #0 + copy(Descricao,1,200) + #0) ;
   end
  else
   begin
     fpArredondaItemMFD := False;
     Codigo := PadRight(Codigo,13) ;
     if Round( Qtd ) = Qtd then
        QtdStr := IntToStrZero( Round( Qtd ), 4)
     else
        QtdStr := IntToStrZero( Round(Qtd * 1000), 7) ;

     if (RoundTo( ValorUnitario, -2 ) = ValorUnitario) then
      begin
        ValorStr := IntToStrZero( Round(ValorUnitario * 100), 8) ;
        CMD := 09 ;
      end
     else
      begin
        ValorStr := IntToStrZero( Round(ValorUnitario * 1000), 8) ;
        CMD := 56 ;
      end ;

     if TipoDescontoAcrescimo='%' then
        DescontoStr := IntToStrZero( Round(ValorDescontoAcrescimo * 100), 4)
      else
        DescontoStr := IntToStrZero( Round(ValorDescontoAcrescimo * 100), 8) ;

     if ( StrToIntDef( NumVersao,0 ) >= 300) and (Trim(Unidade) <> '') then
     begin
        BytesResp := 0 ;
        EnviaComando( #62+#51 + Unidade ) ;            { Programando a UN }
     end ;

     if DescricaoGrande then
        if Length( Descricao ) > 29 then       { Programando aumento de Descricao }
           if (StrToIntDef( NumVersao,0 ) >= 300) then
              EnviaComando( #62+#52 + copy(Descricao,1,200) ) ;

     Descricao := PadRight(Descricao,29) ;
     BytesResp := 0 ;
     EnviaComando(chr(CMD) + Codigo + Descricao + AliquotaECF + QtdStr +
               ValorStr + DescontoStr ) ;
   end ;

  fsTotalPago := 0 ;
  fsTotalizadoresParciais := '' ;
end;

procedure TACBrECFBematech.VendeItemEx(Codigo, Descricao: String;
  AliquotaICMS: String; Qtd: Double; ValorUnitario: Double;
  ValorDescontoAcrescimo: Double; Unidade: String;
  TipoDescontoAcrescimo: String; DescontoAcrescimo: String;
  CodDepartamento: Integer; EAN13: String; CasasDecimaisQtde: Integer;
  CasasDecimaisValor: Integer; ArredondaTrunca: Char; NCM: String;
  CFOP: String; InformacaoAdicional: String; TotalDosTributos: Double;
  OrigemProduto: Integer; CST_ICMS: String; ModalidadeBCICMS: Integer;
  PercentualReducaoBCICMS: Double; CSOSN: String; ValorBaseCalculoSN: Double;
  ValorICMSRetidoSN: Double; AliquotaCalculoCreditoSN: Double;
  ValorCreditoICMSSN: Double; ItemListaServico: String; CodigoISS: String;
  NaturezaOperacaoISS: String; IndicadorIncentivoFiscalISS: Integer;
  CodigoIBGE: String; ModalidadeBCICMSST: Integer;
  PercentualMargemICMSST: Double; PercentualReducaoBCICMSST: Double;
  ValorReducaoBCICMSST: Double; AliquotaICMSST: Double; ValorICMSST: Double;
  ValorICMSDesonerado: Double; MotivoDesoneracaoICMS: Integer; CST_PIS: String;
  BaseCalculoPIS: Double; AliquotaPIS: Double; ValorPIS: Double;
  QuantidadeVendidaPIS: Double; ValorAliquotaPIS: Double; CST_COFINS: String;
  BaseCalculoCOFINS: Double; AliquotaCOFINS: Double; ValorCOFINS: Double;
  QuantidadeVendidaCOFINS: Double; ValorAliquotaCOFINS: Double; CEST: String);
Var
  Comando : String;
  NumItem: Integer;
begin
  // TODO: Dúvidas:
  // InformacaoAdicional, o manual fala 500 e no comando suporta 80;
  // Não há parâmetros no CMD para informar os dados de ISS
  // Quantas decimais considerar nos campos Numericos ?
  
  if Qtd > 9999 then
     raise EACBrECFCMDInvalido.Create( ACBrStr(
           'Quantidade deve ser inferior a 9999.') );

  Comando := PadRight(Codigo, 14) +
             PadRight(EAN13, 13) +
             PadRight(Descricao, 120) +
             PadRight(AliquotaICMS, 2) +
             PadRight(Unidade,2) +
             IntToStrZero( Round( Qtd * power(10, CasasDecimaisQtde)), 7) +
             IntToStrZero( Round( ValorUnitario * power(10, CasasDecimaisValor)), 8) +
             ArredondaTrunca +
             TipoDescontoAcrescimo[1] +
             IntToStrZero( Round( ValorDescontoAcrescimo * 100), 8) +
             PadRight(NCM, 8) +
             PadRight(CFOP, 4) +
             PadRight(CST_ICMS, 2) +
             IntToStr(OrigemProduto) +
             PadRight(CSOSN, 3) +
             IntToStrZero( Round( ValorBaseCalculoSN * 100), 8) +
             IntToStrZero( Round( ValorICMSRetidoSN * 100), 8) +
             PadRight( InformacaoAdicional, 80) +
             DescontoAcrescimo[1] +
             PadLeft(Trim(CodigoIBGE), 7, '0') +
             IntToStr(ModalidadeBCICMS) +
             IntToStrZero( Round( PercentualReducaoBCICMS * 100), 4) +
             IntToStr(ModalidadeBCICMSST) +
             IntToStrZero( Round( PercentualMargemICMSST * 100), 4) +
             IntToStrZero( Round( PercentualReducaoBCICMSST * 100), 4) +
             IntToStrZero( Round( ValorReducaoBCICMSST * 100), 15) +
             IntToStrZero( Round( AliquotaICMSST * 100), 4) +
             IntToStrZero( Round( ValorICMSST * 100), 15) +
             IntToStrZero( Round( ValorICMSDesonerado * 100), 15) +
             IntToStrZero( MotivoDesoneracaoICMS, 2) +
             IntToStrZero( Round( AliquotaCalculoCreditoSN * 100), 4) +
             IntToStrZero( Round( ValorCreditoICMSSN * 100), 15) +
             IntToStrZero( Round( TotalDosTributos * 100), 8) +
             PadRight(CST_PIS, 2) +
             IntToStrZero( Round( BaseCalculoPIS * 100), 15) +
             IntToStrZero( Round( AliquotaPIS * 100), 4) +
             IntToStrZero( Round( ValorPIS * 100), 15) +
             IntToStrZero( Round( QuantidadeVendidaPIS * 100), 15) +
             IntToStrZero( Round( ValorAliquotaPIS * 100), 15) +
             PadRight(CST_COFINS, 2) +
             IntToStrZero( Round( BaseCalculoCOFINS * 100), 15) +
             IntToStrZero( Round( AliquotaCOFINS * 100), 4) +
             IntToStrZero( Round( ValorCOFINS * 100), 15) +
             IntToStrZero( Round( QuantidadeVendidaCOFINS * 100), 15) +
             IntToStrZero( Round( ValorAliquotaCOFINS * 100), 15);             

  BytesResp := 0 ;
  EnviaComando( #10 + Comando );

  fsTotalPago := 0 ;
  fsTotalizadoresParciais := '' ;

  CEST := OnlyNumber(CEST);
  if CEST <> '' then
  begin
    NumItem := GetNumUltimoItem;
    EnviaComando(#62+#95+#67 + IntToStrZero(NumItem, 3) + Poem_Zeros(CEST, 7));
  end;
end;

procedure TACBrECFBematech.DescontoAcrescimoItemAnterior(
   ValorDescontoAcrescimo : Double ; DescontoAcrescimo : String;
   TipoDescontoAcrescimo : String; NumItem : Integer) ;
Var
  ValDescAcresStr: String ;
begin
  if not fs25MFD then
     exit ;

  if NumItem = 0 then
     NumItem := NumUltItem;

  if DescontoAcrescimo <> 'A' then
     DescontoAcrescimo := 'D' ;

  ValDescAcresStr := IntToStrZero( Round(ValorDescontoAcrescimo * 100),
     ifthen(TipoDescontoAcrescimo = '%', 4, 8) ) ;

  EnviaComando( #93 + DescontoAcrescimo +
                IntToStrZero( NumItem, 3) + ValDescAcresStr ) ;
end ;

procedure TACBrECFBematech.CancelaDescontoAcrescimoItem( NumItem: Integer;
   TipoAcrescimoDesconto: String);
begin
  if not fs25MFD then
     exit ;

  if NumItem = 0 then
     NumItem := NumUltItem;

  if TipoAcrescimoDesconto <> 'A' then
     TipoAcrescimoDesconto := 'D' ;

  EnviaComando(#114 + TipoAcrescimoDesconto +  IntToStrZero(NumItem,3));
end;

procedure TACBrECFBematech.CarregaAliquotas;
Var StrRet : AnsiString ;
    Cont, qtdAliq : Integer ;
    Aliquota : TACBrECFAliquota ;
    ValAliq : Double ;
    ByteISS1,ByteISS2,ByteUltimaAliquota : Byte ;
    
begin
  StrRet := RetornaInfoECF( '29' ) ;
  try ByteISS1 := Ord(StrRet[1]) ; except ByteISS1 := 0 ; end ;
  try ByteISS2 := Ord(StrRet[2]) ; except ByteISS2 := 0 ; end ;

  BytesResp := 33 ;
  StrRet := EnviaComando( #26 ) ;

  ByteUltimaAliquota := 0 ;
  StrRet  := BcdToAsc( copy( StrRet, 2, Length(StrRet)) ) ;  { 1o Byte nao é BCD }
  qtdAliq := Trunc(Length(StrRet)/4);

  inherited CarregaAliquotas ;   { Cria fpAliquotas }

  {Procura qual foi a última alíquota cadastrada diferente de alíquota zero}
  for Cont := qtdAliq Downto 1 do
  begin
     ValAliq := RoundTo( StrToIntDef(copy(StrRet,((Cont-1)*4)+1,4),0)/100,-2);
     if ValAliq > 0 then
     begin
        ByteUltimaAliquota := Cont;
        Break;
     end;
  end;

  {Adiciona todas alíquotas até a última cadastrada}
  for Cont := 1 to ByteUltimaAliquota do
  begin
    ValAliq  := RoundTo( StrToIntDef(copy(StrRet,((Cont-1)*4)+1,4),0)/100,-2);

    Aliquota := TACBrECFAliquota.create ;
    Aliquota.Indice   := IntToStrZero(Cont,2) ;
    Aliquota.Aliquota := ValAliq ;

    if Cont < 9 then
    begin
       if TestBit( ByteISS1, 8 -Cont) then
          Aliquota.Tipo := 'S' ;
    end
    else
       if TestBit( ByteISS2, 16-Cont) then
          Aliquota.Tipo := 'S' ;

    fpAliquotas.Add( Aliquota ) ;
  end ;
end;

procedure TACBrECFBematech.LerTotaisAliquota;
Var A : Integer ;
begin
  if not Assigned( fpAliquotas ) then
     CarregaAliquotas ;

  fsTotalizadoresParciais := '';

  For A := 0 to fpAliquotas.Count-1 do
  begin
     fpAliquotas[A].Total := RoundTo( StrToFloatDef( BcdToAsc(
                      copy(TotalizadoresParciais,(A*7)+1,7)),0) / 100, -2) ;
  end ;
end;


procedure TACBrECFBematech.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
Var ValStr : String ;
begin
  { Impressora Bematech não usa o parâmetro Posicao }
  ValStr := IntToStrZero( Round(Aliquota * 100) ,4) ;
  Tipo   := UpCase(Tipo) ;
  if Tipo = 'S' then
     Tipo := '1'
  else
     Tipo := '0' ;

  BytesResp := 0 ;
  EnviaComando( #07 + ValStr + Tipo ) ;
  CarregaAliquotas ;
end;

function TACBrECFBematech.AchaICMSAliquota( var AliquotaICMS: String):
   TACBrECFAliquota;
begin
  if upcase(AliquotaICMS[1]) = 'T' then
    AliquotaICMS := 'T'+PadLeft(copy(AliquotaICMS,2,2),2,'0') ; {Indice}

  Result := inherited AchaICMSAliquota( AliquotaICMS );

  if (pos(AliquotaICMS[1],'FIN') > 0) then
  begin
    if copy(AliquotaICMS,2,1) = 'S' then
      AliquotaICMS  := 'S' + AliquotaICMS[1]                { SN, SF, SI }
    else
      AliquotaICMS := AliquotaICMS[1] + AliquotaICMS[1];    { NN, FF, II }
  end;
end;

procedure TACBrECFBematech.CarregaFormasPagamento;  { funçao Lenta +- 3 sec. }
Var StrRet : AnsiString ;
    Cont : Integer ;
    FPagto : TACBrECFFormaPagamento ;
//    FPagtoUltimoCupom : TACBrECFFormaPagamento ;
    Descr : String ;
begin
  if not fs25MFD then
   begin
     //BytesResp := 1925 ;
     StrRet := RetornaInfoECF( '32' );
     //  1 + (52 * 16) + (52 * 10) + (52 * 10) + (52 * 1)
     //  1 + 832 + 520 + 520 + 52 = 1925

     inherited CarregaFormasPagamento ;   { Cria fpFormasPagamentos }

     For Cont := 1 to 50 do
     begin
       Descr := trim( copy( StrRet, (Cont * 16) - 15 + 1, 16) ) ;
       if Descr <> '' then
       begin
          FPagto := TACBrECFFormaPagamento.create ;

          FPagto.Indice    := IntToStrZero(Cont,2) ;
          FPagto.Descricao := Descr;
          FPagto.PermiteVinculado := (Cont > 1); {Apenas 1-Dinheiro nao permite}
          FPagto.Total := RoundTo( StrToFloatDef( BcdToAsc(
                              copy(StrRet,(Cont*10) - 9 + 833,10) ),0) / 10000, -4) ;

          fpFormasPagamentos.Add( FPagto ) ;
       end ;
     end ;
   end
  else
   begin
     StrRet := RetornaInfoECF( '49' );
     //  (20 * 16) + (20 * 7) + (20 * 7)   + (20 * 1)
     //  Descricao +  Valor   +  Valor ult.+ Flag TEF
     //     320    +   140    +   140      +   20

     inherited CarregaFormasPagamento ;   { Cria fpFormasPagamentos }

     For Cont := 1 to 20 do
     begin
       Descr := trim( copy( StrRet, (Cont * 16) - 15, 16) ) ;
       if Descr <> '' then
       begin
          FPagto := TACBrECFFormaPagamento.create ;

          FPagto.Indice    := IntToStrZero(Cont,2) ;
          FPagto.Descricao := Descr;
          FPagto.PermiteVinculado := (Cont > 1) and
                                     (copy(StrRet, Cont + 600, 1 ) = #85);
          FPagto.Total := RoundTo( StrToFloatDef( BcdToAsc(
                              copy(StrRet,(Cont*7) - 6 + 320,7) ),0) / 100, -4) ;

          // O ECF permite recuperar as formas de pagamento e seus valores do último cupom.
          // TODO: Verificar se outras marcas permitem isso. Em caso afirmativo, adicionar função ou propriedade correspondente.
//          FPagtoUltimoCupom := TACBrECFFormaPagamento.create ;
//          FPagtoUltimoCupom.Descricao := Descr;
//          FPagtoUltimoCupom.Total := RoundTo( StrToFloatDef( BcdToAsc(
//                                                copy(StrRet,(Cont*7) - 6 + 460,7) ),0) / 100, -4) ;

          fpFormasPagamentos.Add( FPagto ) ;
       end ;
     end ;
   end ;

end;

procedure TACBrECFBematech.LerTotaisFormaPagamento;
begin
  CarregaFormasPagamento ;
end;

procedure TACBrECFBematech.CarregaRelatoriosGerenciais;
Var
  RetCmd, Token1, Token2, Descricao : AnsiString ;
  Cont, CER : Integer ;
  RG  : TACBrECFRelatorioGerencial ;
begin
  inherited CarregaRelatoriosGerenciais ;   {Inicializa fpRelatoriosGerenciais}

  try
    if fpMFD then
    begin
      //BytesResp  := 570;
      RetCmd  := RetornaInfoECF( '51' );

      for Cont := 1 to 30 do
      begin
        { Adicionando os Relatorios Gerenciais }
        Token1    := copy(RetCmd, ((Cont-1) * 19) +3, 17) ;
        Descricao := Trim(Token1) ;

        Token2:= BcdToAsc(  copy(RetCmd, ((Cont-1) * 19) + 1, 2) ) ;
        CER   := StrToIntDef(Token2, 0) ;

        if (Descricao <> '') and (Descricao[2] <> #255) then
        begin
          RG := TACBrECFRelatorioGerencial.create ;
          RG.Indice     := IntToStrZero(Cont,2);
          RG.Descricao  := Descricao ;
          RG.Contador   := CER;

          fpRelatoriosGerenciais.Add( RG ) ;
        end ;
      end ;
    end ;
  except
    { Se falhou ao carregar, deve "nilzar" as variaveis para que as rotinas
      "Acha*" tentem carregar novamente }
    fpRelatoriosGerenciais.Free ;
    fpRelatoriosGerenciais := nil ;

    raise ;
  end ;

end;

procedure TACBrECFBematech.LerTotaisRelatoriosGerenciais ;
begin
  CarregaRelatoriosGerenciais;
end ;

function TACBrECFBematech.AchaFPGDescricao(Descricao: String;
  BuscaExata: Boolean; IgnorarCase: Boolean; IgnorarAcentos: Boolean
  ): TACBrECFFormaPagamento;
begin
  { A Bematech permite programas as Formas de Pagamento dinâmicamente.
    Na MP20 A cada Reduçao Z as Formas programadas dinâmicamente sao zeradas.
    O comando abaixo tenta programar novamente a FPG para garantir que ela
    exista... Caso ela já exista a Bematech retorna o INDICE atual...
    - Isso é necessario pois pode haver situacoes que as Formas de Pagamento
      que constam na memoria do ACBr em "fpFormasPagamentos" já não existem mais
      pois uma Reducao Z foi impressa automática pelo ECF }
  { Nas novas MP25 e MPTH as Formas de Pagamento, nao são zeradas na Reducao Z,
    por isso não será utilizado a programação dinâmica }
  if (not fs25MFD) then
     ProgramaFormaPagamento( Descricao ) ;

  result := inherited AchaFPGDescricao(Descricao,BuscaExata,IgnorarCase,IgnorarAcentos) ;
end;

procedure TACBrECFBematech.ProgramaFormaPagamento( var Descricao: String;
  PermiteVinculado : Boolean; Posicao : String) ;
Var StrRet : AnsiString ;
    FPagto : TACBrECFFormaPagamento ;
    VincStr: String ;
begin
  { Impressora Bematech não usa o parâmetro Posicao }

  Descricao := PadRight(Descricao,16) ;         { Ajustando tamanho final }
  VincStr   := '' ;
  if fs25MFD then
   begin
     if PermiteVinculado then
        VincStr := '1'
     else
        VincStr := '2' ;
   end
  else
     { Obs: Bematech MP20 nao usa PermiteVinculado. Apenas Fpagto 01-Dinheiro
           (fixa, sempre existente) nao permite Vinculado }
     PermiteVinculado := True ;

  BytesResp := 2 ;
  StrRet := BcdToAsc( EnviaComando( #71 + Descricao + VincStr ) ) ;
  StrRet := copy(StrRet,2,1)+copy(StrRet,4,1) ;

  { Adicionando nova Forma no ObjectList }
  if (StrToIntDef( StrRet, 0 ) > 0) then
  begin
     if Assigned( fpFormasPagamentos ) then
     begin
        { Bematech retorna o mesmo indice se já existir, verificando se é nova }
        FPagto := AchaFPGIndice(StrRet) ;
        if FPagto = nil then
        begin
           FPagto := TACBrECFFormaPagamento.create ;
           FPagto.Indice    := StrRet ;
           FPagto.Descricao := Trim(Descricao) ;
           FPagto.PermiteVinculado := PermiteVinculado ;
           fpFormasPagamentos.Add( FPagto ) ;
        end ;
        Descricao := FPagto.Descricao ;
     end ;
  end ;
end;

procedure TACBrECFBematech.ProgramaRelatorioGerencial( var Descricao: String; Posicao: String);
Var
  ProxIndice : Integer ;
begin
  CarregaRelatoriosGerenciais ;

  Descricao := Trim(Descricao) ;
  ProxIndice := StrToIntDef(Posicao, -1) ;

  if fpMFD then
   begin
     if AchaRGDescricao(Descricao, True) <> nil then
        raise EACBrECFErro.Create( ACBrStr('Relatório Gerencial ('+Descricao+') já existe.')) ;

     if (ProxIndice < 2) or (ProxIndice > 30) then { Indice passado é válido ? }
     begin
        For ProxIndice := 2 to 30 do  { Procurando Lacuna }
        begin
           if AchaRGIndice(IntToStrZero(ProxIndice,2)) = nil then
              break ;
        end ;
     end ;

     if ProxIndice > 30 then
        raise EACBrECFErro.create( ACBrStr('Não há espaço para programar novos RGs'));

     EnviaComando( #82 + IntToStrZero(ProxIndice,2) + PadRight(Descricao,17) ) ;
   end
  else
     raise EACBrECFErro.Create( ACBrStr('Impressoras sem MFD não suportam Programação de Relatórios Gerenciais'));

  CarregaRelatoriosGerenciais ;
end;

procedure TACBrECFBematech.CarregaComprovantesNaoFiscais;
Var RetCmd, S: AnsiString ;
    Cont  : Integer ;
    CNF   : TACBrECFComprovanteNaoFiscal ;
    Descr : String ;
begin
  Cont      := 1 ;
  //BytesResp := 1550 ;
  RetCmd    := RetornaInfoECF( '33' ); //, 8 ) ;
  // ( 2 * 50 ) + (10 * 50) + (19 * 50)
  //     100    +    500    +   950    = 1550

  inherited CarregaComprovantesNaoFiscais ;

  try
     S := RetCmd ;
     while Length(S) > 0 do
     begin
       Descr  := trim( copy(S, 13,19) ) ;

       if Descr <> '' then
       begin
          CNF := TACBrECFComprovanteNaoFiscal.create ;

          CNF.Indice    := IntToStrZero(Cont,2) ;
          CNF.Descricao := Descr ;
          CNF.Total     := RoundTo( StrToFloatDef(
                                    BcdToAsc(copy(S,3,10)),0) / 10000, -4) ;
          CNF.Contador  := StrToIntDef( BcdToAsc(copy(S,1,2)),0) ;

          fpComprovantesNaoFiscais.Add( CNF ) ;
       end ;

       S    := copy(S, 32, Length(S) ) ;
       Cont := Cont + 1 ;
     end ;

     RetCmd := '' ;
     if fs25MFD then
     begin
        //BytesResp := 60 ;
        RetCmd    := RetornaInfoECF( '47' ) ;
     end ;

    { Adicionando SA-Sangria e SU-Suprimento que sempre estarão presentes na Bematech}
     CNF := TACBrECFComprovanteNaoFiscal.create ;
     CNF.Indice    := 'SA' ;
     CNF.Descricao := 'Sangria' ;
     CNF.Total     := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,ifThen(fs25MFD,393,197),7) ),0) / 100 ;
     CNF.Contador  := StrToIntDef( copy(RetCmd,113,4),0) ;
     fpComprovantesNaoFiscais.Insert(0, CNF ) ;

     CNF := TACBrECFComprovanteNaoFiscal.create ;
     CNF.Indice    := 'SU' ;
     CNF.Descricao := 'Suprimento' ;
     CNF.Total     := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,ifThen(fs25MFD,400,204),7) ),0) / 100 ;
     CNF.Contador  := StrToIntDef( copy(RetCmd,117,4),0) ;
     fpComprovantesNaoFiscais.Insert(1, CNF ) ;
  except
    { Se falhou ao carregar, deve "nilzar" as variaveis para que as rotinas
      "Acha*" tentem carregar novamente }
     fpComprovantesNaoFiscais.Free;
     fpComprovantesNaoFiscais := nil ;

     raise ;
  end ;
end;

procedure TACBrECFBematech.LerTotaisComprovanteNaoFiscal;
begin
  CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFBematech.ProgramaComprovanteNaoFiscal(var Descricao : String;
   Tipo: String; Posicao : String);
Var ProxIndice : Integer ;
    CNF    : TACBrECFComprovanteNaoFiscal ;
begin
  { Obs: Bematech nao usa Tipo }
  Descricao := PadRight(Descricao,19) ;

  CarregaComprovantesNaoFiscais ;

  ProxIndice := StrToIntDef(Posicao,0) ;
  if (ProxIndice < 1) or (ProxIndice > 50) then { Indice passado é válido ? }
     ProxIndice := ComprovantesNaoFiscais.Count + 1 ;

  if ProxIndice > 50 then
     raise EACBrECFErro.create( ACBrStr('Não há espaço para programar novos Comprovantes'+
                            ' não Fiscais'));

  BytesResp := 0 ;
  EnviaComando( #40 + IntToStrZero(ProxIndice,2) + Descricao ) ;

  { Adcionanodo novo CNF no ObjectList }
  CNF := TACBrECFComprovanteNaoFiscal.create ;
  CNF.Indice    := IntToStrZero(ProxIndice,2) ;
  CNF.Descricao := Descricao ;
  fpComprovantesNaoFiscais.Add( CNF ) ;
end;


procedure TACBrECFBematech.ImprimeCheque(Banco: String; Valor: Double;
  Favorecido, Cidade: String; Data: TDateTime; Observacao: String);
Var ValStr, DataStr : String ;
    Modelo  : TACBrCHQModelo ;
begin

  Banco      := IntToStrZero( StrToInt(Banco), 3) ;
  Favorecido := PadRight(Favorecido,45) ;
  Cidade     := PadLeft(trim(Cidade),27) ;
  Observacao := copy(Observacao,1,120) ;
  DataStr    := FormatDateTime('ddmmyyyy',Data) ;
  ValStr     := IntToStrZero( Round(abs(Valor)*100),14) ;

  Modelo := fsModelosCheque.AchaModeloBanco( Banco ) ;
  if Modelo = nil then
     raise EACBrECFErro.create( ACBrStr('Modelo de cheque do Banco: '+Banco+
                            ' não encontrado'));
  BytesResp := 0 ;
  with Modelo do
     EnviaComando( #57 + ValStr + Favorecido + Cidade + DataStr +
                   chr( ColunaValor    ) + chr( ColunaExtenso1   ) +
                   chr( ColunaExtenso2 ) + chr( ColunaFavorecido ) +
                   chr( ColunaLocal    ) +
                   chr( ColunaDia ) + chr( ColunaMes ) + chr( ColunaAno ) +
                   chr( LinhaValor    ) + chr( LinhaExtenso1   ) +
                   chr( LinhaExtenso2 ) + chr( LinhaFavorecido ) +
                   chr( LinhaLocal ) + Observacao )
end;

procedure TACBrECFBematech.CancelaImpressaoCheque;
begin
  BytesResp := 0 ;
  EnviaComando( #62 + #49 ) ;
end;

function TACBrECFBematech.GetChequePronto: Boolean;
Var B : Byte ;
    Resp : AnsiString ;
begin
  BytesResp := 1 ;
  Resp := EnviaComando( #62 + #48 ) ;
  B := ord( Resp[1] ) ;
  Result := not TestBit(B,5) ;
end;

procedure TACBrECFBematech.ImpactoAgulhas( NivelForca : Integer = 2);
Var Value : Integer ;
begin
  if fs25MFD or ( StrToIntDef( NumVersao,0 ) < 310) then
     raise EACBrECFErro.Create( ACBrStr('Comando para aumentar o impacto das agulhas '+
                            'não disponível neste modelo de ECF.')) ;

  Value := min(max(NivelForca,3),1) ;
  EnviaComando( #124 + IntToStr( Value ) ) ;
end;

procedure TACBrECFBematech.AbreRelatorioGerencial(Indice: Integer = 0);
 Var Espera : Integer ;
  IndiceStr : String;
  RG  : TACBrECFRelatorioGerencial;
begin
  Espera := 30 ;
  IndiceStr :=  IntToStrZero(Indice, 2);
  BytesResp := 0 ;
  AguardaImpressao := True ;

  if fpMFD and (Indice > 0) then
   begin
     Espera := 10 ;
     RG  := AchaRGIndice( IndiceStr ) ;
     if RG = nil then
        raise EACBrECFErro.create( ACBrStr('Relatório Gerencial: '+IndiceStr+
                                ' não foi cadastrado.' ));

     EnviaComando( #83 + IndiceStr, Espera);
   end
  else
     EnviaComando(#20, Espera);

end;

procedure TACBrECFBematech.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
Var P, Espera : Integer ;
    Buffer : AnsiString ;
    MaxChars : Integer ;
begin
  Linha := AjustaLinhas( Linha, Colunas );  { Formata as Linhas de acordo com "Coluna" }
  MaxChars := 620 ;  { Bematech aceita no máximo 620 caract. por comando }

  if not fpTermica then   { Se não é Termica, Imprime Linha a Linha }
     ImprimirLinhaALinha( Linha, #20 )
  else
     while Length( Linha ) > 0 do
     begin
        P := Length( Linha ) ;
        if P > MaxChars then    { Acha o fim de Linha mais próximo do limite máximo }
           P := PosLast(#10, LeftStr(Linha,MaxChars) ) ;

        if P = 0 then
           P := Colunas ;

        Buffer := copy( Linha, 1, P)  ;
        Espera := Trunc( CountStr( Buffer, #10 ) / 4) ;

        AguardaImpressao := (Espera > 3) ;
        EnviaComando( #20 + Buffer, Espera ) ;

        { ficou apenas um LF sozinho ? }
        if (P = Colunas) and (RightStr( Buffer, 1) <> #10) and
           (copy( Linha, P+1, 1) = #10) then
           P := P + 1 ;

        Linha  := copy( Linha, P+1, Length(Linha) ) ;   // O Restante
     end ;
end;

procedure TACBrECFBematech.AbreCupomVinculado(COO, CodFormaPagto,
   CodComprovanteNaoFiscal :  String; Valor : Double ) ;
Var FPG : TACBrECFFormaPagamento ;
    StrValor, FPGDesc : String ;
    ComandoCompleto : Boolean ;
begin
  FPG := AchaFPGIndice( CodFormaPagto ) ;

  if FPG = nil then
     raise EACBrECFErro.create( ACBrStr('Forma de Pagamento: '+CodFormaPagto+
                             ' não foi cadastrada.') ) ;

  COO       := Poem_Zeros( trim(COO) ,6) ;
  FPGDesc   := PadRight( CodificarPaginaDeCodigoECF(FPG.Descricao), 16 ) ;
//FPGDesc   := UpperCase(copy(FPGDesc,1,1))+LowerCase(copy(FPGDesc,2,16)) ;
  BytesResp := 0 ;
  ComandoCompleto  := ((Valor > 0) and (fs25MFD or (StrToIntDef( NumVersao,0 ) >= 310) )) ;
  StrValor := IntToStrZero( Round(Valor * 100) ,14) ;

  if ComandoCompleto then
    try
       AguardaImpressao := True ;
       EnviaComando( #66 + FPGDesc + StrValor + COO, 10) ;
    except
       ComandoCompleto := False ;
    end ;

  if not ComandoCompleto then
  begin
     AguardaImpressao := True ;
     EnviaComando( #66 + FPGDesc, 10) ;
  end ;
end;

procedure TACBrECFBematech.LinhaCupomVinculado(Linha: AnsiString);
Var P, Espera : Integer ;
    Buffer : AnsiString ;
    MaxChars : Integer ;
begin
  Linha := AjustaLinhas( Linha, Colunas );  { Formata as Linhas de acordo com "Coluna" }
  MaxChars := 620 ;  { Bematech aceita no máximo 620 caract. por comando }

  if not fpTermica then   { Se não é Termica, Imprime Linha a Linha }
     ImprimirLinhaALinha( Linha, #67 )
  else
     while Length( Linha ) > 0 do
     begin
        P := Length( Linha ) ;
        if P > MaxChars then    { Acha o fim de Linha mais próximo do limite máximo }
           P := PosLast(#10, LeftStr(Linha,MaxChars) ) ;

        Buffer := copy( Linha, 1, P)  ;
        Espera := Trunc( CountStr( Buffer, #10 ) / 4) ;

        AguardaImpressao := (Espera > 3) ;
        EnviaComando( #67 + Buffer, Espera ) ;

        Linha  := copy( Linha, P+1, Length(Linha) ) ;   // O Restante
     end ;
end;

procedure TACBrECFBematech.FechaRelatorio;
begin
  BytesResp := 0 ;
  AguardaImpressao := True ;
  EnviaComando( #21, 10 ) ;  { Fecha o relatorio Gerencial ou Vinculado }
end;

procedure TACBrECFBematech.CorrigeEstadoErro(Reducao: Boolean) ;
begin
  try
     EnviaComando( #70 ) ;
     sleep(200) ;
  except
  end ;

  inherited CorrigeEstadoErro(Reducao) ;

  { Algumas versões da Bematech MP20 as vezes, cancelam o Cupom, mas
    permanecem em estado de Pagamento... Geralmente isso ocorre com o termino
    da bobina durante o cancelamento }
  if Estado in [estVenda, estPagamento] then
  begin
     try AbreCupom except end ;
     try CancelaCupom except end ;
  end;
end;

procedure TACBrECFBematech.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal : Integer; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  Espera := 80 + (ReducaoFinal - ReducaoInicial) ;
  AguardaImpressao := True ;
  Flag := 'I' ;
  if Simplificada then
     Flag := 'i' ;
  EnviaComando( #8 + IntToStrZero(ReducaoInicial,6) +
                     IntToStrZero(ReducaoFinal  ,6) + Flag , Espera ) ;

end;

procedure TACBrECFBematech.LeituraMemoriaFiscal(DataInicial,
   DataFinal: TDateTime; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  Espera := 80 + DaysBetween(DataInicial,DataFinal) ;
  AguardaImpressao := True ;
  Flag := 'I' ;
  if Simplificada then
     Flag := 'i' ;
  EnviaComando( #8 + FormatDateTime('ddmmyy',DataInicial) +
                     FormatDateTime('ddmmyy',DataFinal)   + Flag , Espera ) ;

end;

procedure TACBrECFBematech.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas : TStringList; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  BytesResp := -1 ; { espera por ETX }
  Espera := Trunc(30 + ((ReducaoFinal - ReducaoInicial)/2) ) ;
  Flag := 'R' ;
  if Simplificada then
     Flag := 'r' ;

  Linhas.Clear ;
  Linhas.Text := EnviaComando( #8 + IntToStrZero(ReducaoInicial,6) +
                                    IntToStrZero(ReducaoFinal  ,6) + Flag, Espera ) ;
end;

procedure TACBrECFBematech.LeituraMemoriaFiscalSerial(DataInicial,
   DataFinal: TDateTime; Linhas : TStringList; Simplificada : Boolean);
 Var Espera : Integer ;
     Flag   : Char ;
begin
  BytesResp := -1 ; { espera por ETX }
  Espera := Trunc(30 + (DaysBetween(DataInicial,DataFinal)/2) ) ;
  Flag := 'R' ;
  if Simplificada then
     Flag := 'r' ;

  Linhas.Clear ;
  Linhas.Text := EnviaComando( #8 + FormatDateTime('ddmmyy',DataInicial)+
                                    FormatDateTime('ddmmyy',DataFinal)  +Flag ,Espera);
//WriteToTXT('d:\temp\mfd_limpo.txt',Linhas.Text, False);
end;

procedure TACBrECFBematech.LeituraMFDSerial(COOInicial, COOFinal: Integer;
  Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet);
Var
  Espera : Integer ;
  UsuarioECF : String ;
begin
  { O download da MFD é um processo bastante demorado por isso forcei um TimeOut
    maior. Dependendo da Faixa de COO's a ser baixada pode ser necessário aumantar
    ainda mais o TimeOut. Cfe. testes realizados, faixas de 100 COOs ainda são
    grandes demais p/ um TimeOut de 300 seg., aconselha-se fazer a leitura por
    faixas de 50 em 50 COOs ( Aprox. 220 COOs em 8min em uma FS600 V.1.03) }

  BytesResp := -1 ; { espera por ETX }
  Espera := 30 + (COOFinal - COOInicial) * 2 ;
  UsuarioECF := Prop ;

  Linhas.Clear ;
  Linhas.Text := EnviaComando( #62+#69 + 'C' +
                               IntToStrZero(COOInicial,6) +
                               IntToStrZero(COOFinal  ,6) + UsuarioECF , Espera ) ;
end;

procedure TACBrECFBematech.LeituraMFDSerial(DataInicial,
  DataFinal: TDateTime; Linhas: TStringList; 
  Documentos : TACBrECFTipoDocumentoSet);
Var
  Espera : Integer ;
begin
  BytesResp := -1 ; { espera por ETX }
  Espera := Trunc(30 + (DaysBetween(DataInicial,DataFinal)/2) ) ;

  Linhas.Clear ;
  Linhas.Text := EnviaComando( #62+#69 + 'D' +
                               FormatDateTime('ddmmyy',DataInicial)+
                               FormatDateTime('ddmmyy',DataFinal)  ,Espera);
//WriteToTXT('d:\temp\mfd_limpo.txt',Linhas.Text, False);
end;

function TACBrECFBematech.GetCNPJ: String;
begin
  if fs25MFD then
     Result := RetornaInfoECF( '42' )
  else
     Result := copy(Trim( RetornaInfoECF( '02' ) ),1,18) ;
end;

function TACBrECFBematech.GetIE: String;
begin
  if fs25MFD then
     Result  := RetornaInfoECF( '43' )
  else
     Result  := copy(Trim( RetornaInfoECF( '02' ) ),19,18) ;
end;

function TACBrECFBematech.GetIM: String;
begin
  if fs25MFD then
     Result := RetornaInfoECF( '44' )
  else
     Result := copy(Trim( RetornaInfoECF( '02' ) ),37,18) ;
end;

function TACBrECFBematech.GetCliche: AnsiString;
begin
  Result := RetornaInfoECF( '13' ) ;
end;

function TACBrECFBematech.GetUsuarioAtual: String;
begin
  Result := RetornaInfoECF( '11' ) ;
end;

function TACBrECFBematech.GetDataHoraSB: TDateTime;
begin
  Result := 0;
  // verificar se a redução Z está pendente e não fazer se estiver
  // porque acontecerá erro, conforme consulta ao atendimento da bematech
  if Estado in [estLivre] then
     Result := inherited GetDataHoraSB;
end;

function TACBrECFBematech.GetSubModeloECF: String;

begin
  if fs25MFD then
  begin
    if (fsSubModeloECF = '') then
      fsSubModeloECF := Trim( RetornaInfoECF( '60' ));
  end
  else fsSubModeloECF := '';

  Result := fsSubModeloECF;
end;

function TACBrECFBematech.GetDataMovimento: TDateTime;
Var
  RetCmd : AnsiString ;
begin
   RetCmd := RetornaInfoECF( '27' ) ;

   if RetCmd = '000000' then
      Result := 0.0 //Result := DataHora // removido, porque se retornou zero ainda não teve movimento no dia
   else
      Result := StringToDateTime( copy(RetCmd, 1,2) + DateSeparator +
                                  copy(RetCmd, 3,2) + DateSeparator +
                                  copy(RetCmd, 5,2), 'dd/mm/yy' );
end;

function TACBrECFBematech.GetDataHoraUltimaReducaoZ : TDateTime ;
Var
  RetCmd : AnsiString ;
begin
  RetCmd := RetornaInfoECF( '26' ) ;
  if copy(RetCmd,1,6) = '000000' then
     Result := 0
  else
     Result := StringToDateTime( copy(RetCmd, 1,2) + DateSeparator +
                                 copy(RetCmd, 3,2) + DateSeparator +
                                 copy(RetCmd, 5,2) + ' ' +
                                 copy(RetCmd, 7,2) + TimeSeparator +
                                 copy(RetCmd, 9,2) + TimeSeparator +
                                 copy(RetCmd,11,2),
                                 'dd/mm/yy hh:nn:ss' ) ;
end ;

function TACBrECFBematech.GetGrandeTotal: Double;
begin
  Result := StrToFloatDef(  RetornaInfoECF( '03' )  ,0) / 100 ;
  Result := RoundTo( Result, -2) ;
end;

function TACBrECFBematech.GetNumCRZ: String;
begin
  Result := RetornaInfoECF( '09' ) ;
end;

function TACBrECFBematech.GetNumNCN: String;
begin
  Result := RetornaInfoECF( '57' ) ;
end;

function TACBrECFBematech.GetNumCCDC: String;
begin
  Result := '0000' ; //Ainda não descobrimos como essa informação pode ser retornada fora da DadosdaUltimaReducaoZ
end;

function TACBrECFBematech.GetTotalAcrescimos: Double;
begin
  Result := StrToFloatDef( RetornaInfoECF( '30' ) ,0) / 100 ;
  Result := RoundTo( Result, -2) ;
end;

function TACBrECFBematech.GetTotalAcrescimosISSQN: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,183,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalAcrescimosOPNF: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,414,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalCancelamentos: Double;
begin
  Result := StrToFloatDef( RetornaInfoECF( '04' ) ,0) / 100 ;
  Result := RoundTo( Result, -2) ;
end;

function TACBrECFBematech.GetTotalCancelamentosISSQN: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,190,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalCancelamentosOPNF: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,421,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalDescontos: Double;
begin
  Result := StrToFloatDef( RetornaInfoECF( '05' ) ,0 ) / 100 ;
  Result := RoundTo( Result, -2) ;
end;

function TACBrECFBematech.GetTotalDescontosOPNF: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,407,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalTroco: Double;
var
  Cont : Integer ;
  StrRet : AnsiString ;
begin
   // Result:= 0 ; // O Comando para retornar essa informação está defasado de acordo com a documentação. Mas está comentado abaixo pois não funciona no Emulador
  Cont      := 52;
  BytesResp := 1925 ;
  StrRet := EnviaComando( #35+#32, 8 ) ;
  Result := RoundTo( StrToFloatDef( BcdToAsc(
                     copy(StrRet,(Cont*10) - 9 + 833,10) ),0) / 10000, -4) ;
//begin
//  O comando abaixo só funciona nas MP-4000. 
//  Result := StrToFloatDef( RetornaInfoECF( '78' ) ,0) / 100 ;
end;

function TACBrECFBematech.GetTotalDescontosISSQN: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,176,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalIsencao: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,113,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalIsencaoISSQN: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,134,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalizadoresParciais : String ;
begin
  if fsTotalizadoresParciais = '' then
  begin
    if (fpMFD) or (fs25MFD) then
    begin
      BytesResp := 436 ;
      fsTotalizadoresParciais := EnviaComando( #87, 5 )
    end
    else
    begin
      BytesResp := 219 ;
      fsTotalizadoresParciais := EnviaComando( #27, 15 )
    end;
  end;

  Result := fsTotalizadoresParciais ;
end;

function TACBrECFBematech.GetTotalNaoTributado: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,120,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalNaoTributadoISSQN: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,141,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalSubstituicaoTributaria: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,127,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetTotalSubstituicaoTributariaISSQN: Double;
begin
  Result := StrToFloatDef( BcdToAsc( copy(TotalizadoresParciais,148,7) ),0) / 100 ;
end;

function TACBrECFBematech.GetNumUltimoItem: Integer;
begin
  Result := StrToIntDef( RetornaInfoECF( '12' ), 0)  ;
end;

function TACBrECFBematech.GetVendaBruta: Double;
Var RetCmd : AnsiString ;
begin
  try
     // Lendo Grande Total da última Redução Z //
     BytesResp := 308 ;
     RetCmd    := BcdToAsc(EnviaComando( #62 + #55, 5 )) ;
     Result    := StrToFloatDef(copy(RetCmd,3,18),0) / 100;

     fsNumCOOInicial := IntToStr(StrToIntDef(copy(RetCmd,569,6),0)+1);
  except
     Result := 0 ;
  end ;

  Result :=  RoundTo( GrandeTotal - Result, -2) ;
end;

function TACBrECFBematech.GetNumReducoesZRestantes: String;
var
  RetCmd: AnsiString ;
begin
  Result := '';

  if fpMFD then
    RetCmd := RetornaInfoECF( '59' ) ;

  Result := RetCmd;
end;

function TACBrECFBematech.GetNumCOOInicial: String;
Var
  Erro : Boolean ;
  RetCmd : AnsiString ;
  NumUltCOORzAnt: Integer;
begin
  Erro := False ;
  
  if fpMFD then
  begin
     try
        { Comando disponivel epenas a partir da MP2100 }
        RetCmd := RetornaInfoECF( '72' ) ;
        NumUltCOORzAnt := StrToIntDef( copy( RetCmd ,7,6), 0 ) ;

        if NumUltCOORzAnt > 0 then
           NumUltCOORzAnt := min( NumUltCOORzAnt + 2, StrToIntDef(GetNumCupom,0) );

        if NumUltCOORzAnt > 0 then
           fsNumCOOInicial := IntToStrZero( NumUltCOORzAnt, 6 );
     except
        Erro := True ;
     end ;
  end ;

  if Erro or (fsNumCOOInicial = '') then
     GetVendaBruta ;

  Result := fsNumCOOInicial ;
end ;

procedure TACBrECFBematech.NaoFiscalCompleto(CodCNF: String; Valor: Double;
  CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer);
  Var FPG : TACBrECFFormaPagamento ;
begin
  fsTotalizadoresParciais := '' ;
  if fs25MFD then
     inherited NaoFiscalCompleto(CodCNF, Valor, CodFormaPagto, Obs)
     
  else
   begin
     FPG := AchaFPGIndice(CodFormaPagto) ;
     if FPG = nil then
        raise EACBrECFErro.create( ACBrStr('Forma de pagamento: '+CodFormaPagto+
                                ' não encontrada'));

     AguardaImpressao := True ;
     EnviaComando( #25 + CodCNF + IntToStrZero(Round(Valor * 100) ,14) +
                         PadRight(FPG.Descricao,16), 13 ) ;
   end ;
end;

procedure TACBrECFBematech.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  if fs25MFD then
  begin
     AguardaImpressao := True ;
     CPF_CNPJ := TrimRight(CPF_CNPJ) ;
     if CPF_CNPJ <> '' then
     begin
        CPF_CNPJ := PadRight(CPF_CNPJ,29) ;

        if Nome <> '' then
        begin
           Nome := PadRight(Nome,30) ;

           if Endereco <> '' then
              Endereco := PadRight(Endereco,80) ;
        end ;
     end ;

     EnviaComando( #77 + CPF_CNPJ + Nome + Endereco , 5 ) ;
  end ;

  { Linhas acrescentadas por Marciano Lizzoni }
  fsNFValor  := 0;
  fsNFCodCNF := '';
  fsTotalizadoresParciais := '' ;
end;

procedure TACBrECFBematech.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString = '');
begin
  if fs25MFD then
   begin
     if CodCNF = 'SA' then
        CodCNF := '29'
     else if CodCNF = 'SU' then
        CodCNF := '30' ;

     EnviaComando( #78 + CodCNF + IntToStrZero(Round(Valor * 100) ,14)) ;
     fsNFValor  := 0 ;
     fsNFCodCNF := '' ;
   end
  else
   begin
     if fsNFCodCNF <> '' then
        raise EACBrECFErro.Create( ACBrStr('Essa versão de ECF apenas permite o registro '+
                               'de 1 Item não Fiscal'));

     fsNFCodCNF := CodCNF ;
     fsNFValor  := Valor ;
   end ;

  fsTotalizadoresParciais := '' ;
end;

procedure TACBrECFBematech.EfetuaPagamentoNaoFiscal(CodFormaPagto: String;
  Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
begin
  fsNFCodFPG := '' ;
  if fs25MFD then
     EfetuaPagamento(CodFormaPagto, Valor, Observacao, ImprimeVinculado)
  else
     fsNFCodFPG := CodFormaPagto ;
  fsTotalizadoresParciais := '' ;
end;

procedure TACBrECFBematech.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
  Var A_D : Char ;
begin
  if not fs25MFD then
  begin
     fsNFValor := max( fsNFValor + DescontoAcrescimo, 0) ;
     exit ;
  end ;
     
  if DescontoAcrescimo < 0 then
     A_D := 'd'
  else
     A_D := 'a' ;

  DescontoAcrescimo := abs(DescontoAcrescimo) ;

  { Inicia fechamento com formas de Pagamento }
  BytesResp  := 0 ;
  EnviaComando( #79 + A_D +
                IntToStrZero( Round( DescontoAcrescimo * 100),14) );
  fsTotalizadoresParciais := '' ;
end;

procedure TACBrECFBematech.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
begin
  if fs25MFD then
     FechaCupom(Observacao)
  else
     if (fsNFCodFPG <> '') and (fsNFCodCNF <> '') and (fsNFValor > 0) then
        NaoFiscalCompleto(fsNFCodCNF, fsNFValor, fsNFCodFPG, Observacao );
  fsTotalizadoresParciais := '' ;
end;

procedure TACBrECFBematech.CancelaNaoFiscal;
begin
  if fs25MFD then
     EnviaComando( #81 ) ;
end;

function TACBrECFBematech.GetDadosUltimaReducaoZ: String;
Var
  RetCmd, S, SS : AnsiString ;
  I, P : Integer ;
  ECFCRZ, ECFCRO : String;
  AliqZ: TACBrECFAliquota;
  CNFZ: TACBrECFComprovanteNaoFiscal;
  RGZ : TACBrECFRelatorioGerencial;
  DHUltZ : TDateTime ;
begin
  // Zerar variaveis e inicializa Dados do ECF //
  InitDadosUltimaReducaoZ;

  if not Assigned( fpAliquotas ) then
    CarregaAliquotas ;

  if not Assigned( fpComprovantesNaoFiscais ) then
    CarregaComprovantesNaoFiscais ;

  if fpMFD then
  begin

    // Dados dos Relatorios Gerenciais //
    if not Assigned( fpRelatoriosGerenciais ) then
      CarregaRelatoriosGerenciais ;

    with TACBrECF(fpOwner) do
    begin
      DHUltZ := DataHoraUltimaReducaoZ;
    end;

    ComandoLOG := 'GetDadosUltimaReducaoZ_MFD';
    BytesResp := 621 ;
    RetCmd    := BcdToAsc(EnviaComando( #88, 5 )) ;

    {ESC 88 Tamanho de Retorno: 621 bytes (BCD), com a seguinte estrutura:
    Descrição             Bytes         (Digitos BCD)
    RZautomática se zero indica que a RZ foi emitida por comando 1 (2)  1   ,  2
    CRO Contador de Reinício de Operação 2 (4)                          3   ,  6
    CRZ Contador de Redução Z 2 (4)                                     7   , 10
    COO Contador de Ordem de Operação 3 (6)                             11  , 16
    GNF Contador Geral de Operações Não Fiscais 3 (6)                   17  , 22
    CCF Contador de Cupom Fiscal 3 (6)                                  23  , 28
    GRG Contador Geral de Relatório Gerencial 3 (6)                     29  , 34
    CFD Contador de Fita Detalhe Emitida 3 (6)                          35  , 40
    NFC Contador de Operação Não Fiscal Cancelada 2 (4)                 41  , 44
    CFC Contador de Cupom Fiscal Cancelado 2 (4)                        45  , 48
    CON[30] Contadores Específicos de Operações não Fiscais 30x2 (30x4) 49  ,168
    CER[30] Contadores Específicos de Relatórios Gerenciais 30x2 (30x4) 169 ,288
    CDC Contador de Comprovantes de Débito ou Crédito 2 (4)             289 ,292
    NCN Contador de Débito ou Crédito não Emitidos 2 (4)                293 ,296
    CCDC Contador de Débito ou Crédito Cancelados 2 (4)                 297 ,300
    GT Totalizador Geral 9 (18)                                         301 ,318
    TP[16] Totalizadores Parciais Tributados 16x7 (16x14)               319 ,542
    I I Totalizador de Isenção de ICMS 7 (14)                           543 ,556
    NN Totalizador de Não Incidência de ICMS 7 (14)                     557 ,570
    FF Totalizador de Substituição Tributária de ICMS 7 (14)            571 ,584
    SI Totalizador de Isenção de ISSQN 7 (14)                           585 ,598
    SN Totalizador de Não Incidência de ISSQN 7 (14)                    599 ,612
    SF Totalizador de Substituição Tributária de ISSQN 7 (14)           613 ,626
    Totalizador de Desconto em ICMS 7 (14)                              627 ,640
    Totalizador de Desconto em ISSQN 7 (14)                             641 ,654
    Totalizador de Acrécimo em ICMS 7 (14)                              655 ,668
    Totalizador de Acrécimo em ISSQN 7 (14)                             669 ,682
    Totalizador de Cancelamentos em ICMS 7 (14)                         683 ,696
    Totalizador de Cancelamentos em ISSQN 7 (14)                        697 ,710
    TPNS Totalizadores Parciais Não sujeitos ao ICMS 28x7 (28x14)       711 ,1102
    Sangria Totalizador de Sangria 7 (14)                               1103,1116
    Suprimento Totalizador de Suprimento 7 (14)                         1117,1130
    Totalizador de Cancelamentos de Não Fiscais 7 (14)                  1131,1144
    Totalizador de Descontos de Não Fiscais 7 (14)                      1145,1158
    Totalizador de Acrécimos de Não Fiscais 7 (14)                      1159,1172
    Alíquotas Tributadas 16x2 (16x4)                                    1173,1236
    Data do Movimento 3     (6)                                         1237,1242}

    { Alimenta a class com os dados atuais do ECF }
    with fpDadosReducaoZClass do
    begin
      DataHoraEmissao := DHUltZ;
      DataDoMovimento := StringToDateTimeDef( copy(RetCmd,1237,2) + DateSeparator +
                                              copy(RetCmd,1239,2) + DateSeparator +
                                              copy(RetCmd,1241,2), 0, 'dd/mm/yy' );

      CRO  := StrToIntDef( copy(RetCmd,  3,4), 0) ;
      CRZ  := StrToIntDef( copy(RetCmd,  7,4), 0) ;
      COO  := StrToIntDef( copy(RetCmd, 11,6), 0) ;
      GNF  := StrToIntDef( copy(RetCmd, 17,6), 0) ;
      CCF  := StrToIntDef( copy(RetCmd, 23,6), 0) ;
      GRG  := StrToIntDef( copy(RetCmd, 29,6), 0) ;
      CFD  := StrToIntDef( copy(RetCmd, 35,6), 0) ;
      GNFC := StrToIntDef( copy(RetCmd, 41,4), 0) ;
      CFC  := StrToIntDef( copy(RetCmd, 45,4), 0) ;
      GNF  := StrToIntDef( copy(RetCmd, 17,6), 0) ;
      CDC  := StrToIntDef( copy(RetCmd,289,4), 0) ;
      NCN  := StrToIntDef( copy(RetCmd,293,4), 0) ;
      CCDC := StrToIntDef( copy(RetCmd,297,4), 0) ;

      ValorGrandeTotal := RoundTo( StrToFloatDef( copy(RetCmd,301,18),0) / 100, -2) ;
      IsentoICMS := RoundTo( StrToFloatDef( copy(RetCmd,543,14),0) / 100, -2) ;
      NaoTributadoICMS := RoundTo( StrToFloatDef( copy(RetCmd,557,14),0) / 100, -2) ;
      SubstituicaoTributariaICMS := RoundTo( StrToFloatDef( copy(RetCmd,571,14),0) / 100, -2) ;
      IsentoISSQN := RoundTo( StrToFloatDef( copy(RetCmd,585,14),0) / 100, -2) ;
      NaoTributadoISSQN := RoundTo( StrToFloatDef( copy(RetCmd,599,14),0) / 100, -2) ;
      SubstituicaoTributariaISSQN := RoundTo( StrToFloatDef( copy(RetCmd,613,14),0) / 100, -2) ;
      DescontoICMS := RoundTo( StrToFloatDef( copy(RetCmd,627,14),0) / 100, -2)  ;
      DescontoISSQN := RoundTo( StrToFloatDef( copy(RetCmd,641,14),0) / 100, -2)  ;
      AcrescimoICMS  := RoundTo( StrToFloatDef( copy(RetCmd,655,14),0) / 100, -2) ;
      AcrescimoISSQN := RoundTo( StrToFloatDef( copy(RetCmd,669,14),0) / 100, -2);
      CancelamentoICMS := RoundTo( StrToFloatDef( copy(RetCmd,683,14),0) / 100, -2)  ;
      CancelamentoISSQN := RoundTo( StrToFloatDef( copy(RetCmd,697,14),0) / 100, -2)  ;
      DescontoOPNF := RoundTo( StrToFloatDef( copy(RetCmd,1131,14),0) / 100, -2)  ;
      AcrescimoOPNF := RoundTo( StrToFloatDef( copy(RetCmd,1145,14),0) / 100, -2)  ;
      CancelamentoOPNF := RoundTo( StrToFloatDef( copy(RetCmd,1159,14),0) / 100, -2) ;

      // Dados das Aliquotas //
      S := copy(RetCmd,319,224) ; // TP[16] * 14 Totalizadores Parciais Tributados
      For I := 0 to fpAliquotas.Count-1 do
      begin
        AliqZ := TACBrECFAliquota.Create ;
        AliqZ.Assign( fpAliquotas[I] );
        AliqZ.Total := RoundTo( StrToFloatDef( copy(S,(I*14)+1,14),0) / 100, -2);

        AdicionaAliquota( AliqZ );
      end ;

      // Dados dos Comprovantes não Fiscais //
      S :=  copy(RetCmd,1103,28) +  // Sangria(14) + Suprimento(14)
            copy(RetCmd,711,392) ;  // Não ICMS  (392)  28 * 14
      SS := copy(RetCmd,161,8)   +  // Contadores: Sangria(4) + Suprimento(4)
            copy(RetCmd,49,112);    // Não ICMS(112) 28 * 4

      for I := 0 to fpComprovantesNaoFiscais.Count - 1 do
      begin
        CNFZ := TACBrECFComprovanteNaoFiscal.Create ;
        CNFZ.Assign( fpComprovantesNaoFiscais[I] );
        P := StrToIntDef(CNFZ.Indice,0);
        if P = 0 then
          P := I
        else
          P := P + 1;

        CNFZ.Total    := RoundTo( StrToFloatDef( copy(S,(P*14)+1,14),0) / 100, -2) ;
        CNFZ.Contador := StrToIntDef( copy(SS,(P*4)+1,4), 0);

        TotalizadoresNaoFiscais.Add( CNFZ ) ;
      end;

      S := copy(RetCmd,169,120) ; // 30 * 4
      For I := 0 to fpRelatoriosGerenciais.Count-1 do
      begin
        RGZ := TACBrECFRelatorioGerencial.Create ;
        RGZ.Assign( fpRelatoriosGerenciais[I] );
        RGZ.Contador := StrToIntDef(copy(S,(I*4)+1,4), 0) ;

        RelatorioGerencial.Add( RGZ ) ;
      end ;

      CalculaValoresVirtuais;
      Result := MontaDadosReducaoZ;
    end ;

  end
  else //Não é impressora MFD
  begin
    with TACBrECF(fpOwner) do
    begin
      ECFCRZ := NumCRZ;
      ECFCRO := NumCRO;
    end;

    ComandoLOG := 'GetDadosUltimaReducaoZ';
    BytesResp := 308 ;
    RetCmd    := BcdToAsc(EnviaComando( #62 + #55, 5 )) ;

  { ESC 62 55 - Tamanho de Retorno 616 dígitos BCD (308 bytes),
                com a seguinte estrutura.
    2 RZAUT Se 00 redução por comando, caso contrário automática.
   18 GTDA GT no momento da última redução.
   14 CANCEL Cancelamentos
   14 DESCON Descontos
   64 TR Tributos
  266 TP Totalizadores Parciais Tributados
   14 SANGRIA Sangria
   14 SUPRIMENTOS Suprimentos
  126 NSI Totalizadores não Sujeitos ao ICMS
   36 CNSI Contadores dos TP’s não Sujeitos ao ICMS
    6 COO Contador de Ordem de Operação
    6 CNS Contador de Operações não Sujeitas ao ICMS
    2 AL Número de Alíquotas Cadastradas
    6 DATA_PC Data do Movimento
   14 ACRESC Acréscimo
   14 ACRFIN Acréscimo Financeiro

  RRGGGGGGGGGGGGGGGGGGCCCCCCCCCCCCCCDDDDDDDDDDDDDDT001T002T003T004T005T006T007T008T009T010T011T012T013T014T015T016TPT00000000001TPT00000000002TPT00000000003TPT00000000004TPT00000000005TPT00000000006TPT00000000007TPT00000000008TPT00000000009TPT00000000010TPT00000000011TPT00000000012TPT00000000013TPT00000000014TPT00000000015TPT00000000016IIIIIIIIIIIIIINNNNNNNNNNNNNNFFFFFFFFFFFFFFAAAAAAAAAAAAAAUUUUUUUUUUUUUUTNS00000000001TNS00000000002TNS00000000003TNS00000000004TNS00000000005TNS00000000006TNS00000000007TNS00000000008TNS00000000009CN01CN02CN03CN04CN05CN06CN07CN08CN09COOCOOCNSCNSALDTMOVTAAAAAAAAAAAAAAFFFFFFFFFFFFFF
  0000000000000014231000000000000000000000000000001800021605001200050025000250180013001600170002110200100006000100000000000001000000000000020000000000000300000000000004010000000000050100000000000601000000000007010000000000080100000000000901000000000010010000000000110200000000001202000000000013020000000000140200000000001502000000000016020000000001001400000000010114000000000408640000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000162000019161708070000000000011100000000000000
  ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+.
  }

    { Alimenta a class com os dados atuais do ECF }
    with fpDadosReducaoZClass do
    begin
      DataDoMovimento := StringToDateTime( copy(RetCmd,583,2) + DateSeparator +
                                           copy(RetCmd,585,2) + DateSeparator +
                                           copy(RetCmd,587,2), 'dd/mm/yy' );

      CRO  := StrToIntDef( ECFCRO, 0) ;
      CRZ  := StrToIntDef( ECFCRZ, 0) ;
      COO  := StrToIntDef( copy(RetCmd,569,6), 0) ;

      SubstituicaoTributariaICMS := RoundTo( StrToFloatDef( copy(RetCmd,365,14),0) / 100, -2) ;
      NaoTributadoICMS := RoundTo( StrToFloatDef( copy(RetCmd,351,14),0) / 100, -2) ;
      IsentoICMS := RoundTo( StrToFloatDef( copy(RetCmd,337,14),0) / 100, -2) ;
      DescontoICMS := RoundTo( StrToFloatDef( copy(RetCmd,35,14),0) / 100, -2) ;
      CancelamentoICMS := RoundTo( StrToFloatDef( copy(RetCmd,21,14),0) / 100, -2) ;
      AcrescimoICMS := RoundTo( StrToFloatDef( copy(RetCmd,589,14),0) / 100, -2) ;
      ValorGrandeTotal := RoundTo( StrToFloatDef( copy(RetCmd,3,18),0) / 100, -2);

      // Dados das Aliquotas //
      S := copy(RetCmd,113,224);   // 16 * 14
      For I := 0 to fpAliquotas.Count-1 do
      begin
        AliqZ := TACBrECFAliquota.Create ;
        AliqZ.Assign( fpAliquotas[I] );
        AliqZ.Total := RoundTo( StrToFloatDef( copy(S,(I*14)+1,14),0) / 100, -2) ;

        AdicionaAliquota( AliqZ );
      end ;

      S := copy(RetCmd,379,154) ;//Começa na posição 379 para pegar os dados da Sangria e Suprimento, deveria então ter tamanho (14+14+126)
      for I := 0 to fpComprovantesNaoFiscais.Count - 1 do
      begin
        CNFZ := TACBrECFComprovanteNaoFiscal.Create ;
        CNFZ.Assign( fpComprovantesNaoFiscais[I] );
        CNFZ.Total := RoundTo( StrToFloatDef( copy(S,(I*14)+1,14),0) / 100, -2) ;

        TotalizadoresNaoFiscais.Add( CNFZ ) ;
      end;

      CalculaValoresVirtuais;
      Result := MontaDadosReducaoZ;
    end;
  end;
end;

procedure TACBrECFBematech.CortaPapel(const CorteParcial: Boolean);
begin
  if not fpMFD then
     inherited CortaPapel
  else
   begin
     if Estado = estRelatorio then
     begin
        try
           if CorteParcial then
              LinhaRelatorioGerencial( #27 + #109 )
           else
              LinhaRelatorioGerencial( #27 + #119 );

           Sleep( 100 ) ;
        except
           if TestBit( fsST1, 2) then    // comando inexistente ?
              inherited CortaPapel
           else
              raise ;
        end ;
     end ;
   end ;
end;

procedure TACBrECFBematech.IdentificaPAF(NomeVersao, MD5 : String);
begin
  fsPAF := PadRight(MD5,42) + PadRight(NomeVersao,42) ;
  EnviaComando(#62 + #64 + fsPAF) ;
end;

function TACBrECFBematech.GetPAF: String;
begin
  Result := fsPAF ;
end;

function TACBrECFBematech.GetParamDescontoISSQN: Boolean;
Var
  RetCmd : AnsiString ;
  B : Byte ;
begin
  if MFD then
  begin
    RetCmd := RetornaInfoECF( '76' );
    try
      B := ord( RetCmd[1] )
    except
      B := 0
    end;

    Result := TestBit( B ,4)
  end
  else
    Result := False;
end;

function TACBrECFBematech.GetNumCDC: String;
begin
  Result := '';
  if fpMFD then
  begin
    try
      { Comando disponivel apenas a partir da MP2100 }
      Result := RetornaInfoECF( '52' ) ;
    except
    end;
  end;
end;

function TACBrECFBematech.GetNumCFC: String;
begin
  Result := RetornaInfoECF( '08' ) ;
end;

function TACBrECFBematech.GetNumGNF: String;
begin
  Result := RetornaInfoECF( '07' ) ;
end;

function TACBrECFBematech.GetNumGNFC: String;
begin
  Result := RetornaInfoECF( '53' ) ;
end;

function TACBrECFBematech.GetNumCFD: String;
begin
  Result := RetornaInfoECF( '56' ) ;
end;

function TACBrECFBematech.GetNumGRG: String;
begin
  Result := '';
  if fpMFD then
  begin
     try
       { Comando disponivel apenas a partir da MP2100 }
       Result := RetornaInfoECF( '54' ) ;
     except
     end;
  end;
end;

function TACBrECFBematech.RetornaInfoECF(Registrador: String): AnsiString;
 Var ByteReg : Byte ;
     IsBCD   : Boolean ;
     Resp    : AnsiString ;
begin
  {Nota: Ainda é preciso implementar o tratamento para os Registradores 4A e 4B}
  IsBCD     := True ;
  BytesResp := 2 ;

  ByteReg := StrToIntDef( Registrador, 0 ) ;

  Case ByteReg of
     //Notas:
     //  * Conforme os manuais da Bematech (ECFs MP2100, MP3000 e MP4000),
     //      os valores dos registradores 0, 1, 2, 32, 33 e 34 só devem ser
     //      usados para impressoras MP20 FI II e MP40 FI II.
     //  * Apesar de documentados na MP4000 FI registradores 80,81,82 não funcionam
     //      no emulador nem na MP3000. É preciso testar numa MP4000 FI.
     //  * No manual da MP4000 FI são mostrados registradores 4A e 4B que são os mesmos que
     //      os registradores 74 e 75 respectivamente.
     //
     0            : begin BytesResp := 15 ; IsBCD := False ; end ;
     //1,8,9,10,11,12,14,15,18,19,45,46,52,53,57,59,71 : BytesResp := 2 ; //Não são necessários pois o padrão é esse...
     2            : begin BytesResp := 33 ; IsBCD := False ; end ;
     3,68         : BytesResp := 9 ;
     4,5,22,30,66,77,78,79,80,81,82 : BytesResp := 7 ;
     6,7,27,31,41,54,55,56,67,253 : BytesResp := 3 ;
     13           : begin BytesResp := 186 ; IsBCD := False ; end ;
     16,29,70     : IsBCD := False ;
     17,20,21,28,65,74,75,76,254 : begin BytesResp := 1 ; IsBCD := False ; end ;
     23,26,64,72  : BytesResp := 6 ;
     24,73        : BytesResp := 18 ;
     25           : BytesResp := 171 ;
     32           : begin BytesResp := 1925 ; IsBCD := False ; end ;
     33           : begin BytesResp := 1550 ; IsBCD := False ; end ;
     34           : begin BytesResp := 600  ; IsBCD := False ; end ;
     40,42,43,44,58 : begin BytesResp := 20 ; IsBCD := False ; end ;
     47           : BytesResp := 60 ;
     48,51        : begin BytesResp := 570; IsBCD := False ; end ;
     49           : begin BytesResp := 620; IsBCD := False ; end ;
     50           : begin BytesResp := 780; IsBCD := False ; end ;
     60           : begin BytesResp := 42 ; IsBCD := False ; end ;
     61           : begin BytesResp := 6 ; IsBCD := False ; end ;
     62,63        : begin BytesResp := 10 ; IsBCD := False ; end ;
  end ;

  Resp := EnviaComando( #35 + chr(ByteReg) ) ;

  if IsBCD then
     Result := Trim( BcdToAsc( Resp ) )
  else
     Result := Resp ;
end;

procedure TACBrECFBematech.LoadDLLFunctions;

 procedure BematechFunctionDetect( FuncName: String; var LibPointer: Pointer;
    LibName : String = cLIB_Bema ) ;
 var
   sLibName: string;
 begin
   if not Assigned( LibPointer )  then
   begin
     // Verifica se exite o caminho das DLLs
     sLibName := '';
     if Length(PathDLL) > 0 then
        sLibName := PathWithDelim(PathDLL);

     // Concatena o caminho se exitir mais o nome da DLL.
     sLibName := sLibName + LibName;

     if not FunctionDetect( sLibName, FuncName, LibPointer) then
     begin
        LibPointer := NIL ;
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao carregar a função:'+FuncName+' de: '+LibName ) ) ;
     end ;
   end ;
 end ;

var
  VersaoAtual: AnsiString;
  Resp : Integer ;
begin
   BematechFunctionDetect( 'Bematech_FI_VersaoDll',@xBematech_FI_VersaoDll );
   VersaoAtual := StringOfChar(' ',10) ;
   Resp := xBematech_FI_VersaoDll( VersaoAtual ) ;
   if Resp = 1 then
   begin
     VersaoAtual := StringReplace( Trim(VersaoAtual), ',', '.', [rfReplaceAll] ) ;
     if CompareVersions(VersaoAtual, cLIB_VersaoMinima) < 0 then
        raise EACBrECFErro.Create( ACBrStr('A versão de '+cLIB_Bema+' é: '+VersaoAtual+sLineBreak+
                                           'Você deve atualizar para no mímimo: '+cLIB_VersaoMinima)   );
   end;


   BematechFunctionDetect( 'Bematech_FI_ReloadINIFile',@xBematech_FI_ReloadINIFile );
   BematechFunctionDetect( 'Bematech_FI_AbrePortaSerial',@xBematech_FI_AbrePortaSerial );
   BematechFunctionDetect( 'Bematech_FI_FechaPortaSerial',@xBematech_FI_FechaPortaSerial );
   BematechFunctionDetect( 'Bematech_FI_ArquivoMFDPath',@xBematech_FI_ArquivoMFDPath );
   BematechFunctionDetect( 'Bematech_FI_EspelhoMFD',@xBematech_FI_EspelhoMFD );
   BematechFunctionDetect( 'Bematech_FI_GeraRegistrosCAT52MFD',@xBematech_FI_GeraRegistrosCAT52MFD );
   BematechFunctionDetect( 'Bematech_FI_DownloadMFD',@xBematech_FI_DownloadMFD );
   BematechFunctionDetect( 'Bematech_FI_DownloadMF',@xBematech_FI_DownloadMF );
   BematechFunctionDetect( 'Bematech_FI_RelatorioSintegraMFD',@xBematech_FI_RelatorioSintegraMFD );
   BematechFunctionDetect( 'Bematech_FI_GeraRegistrosSpedCompleto',@xBematech_FI_GeraRegistrosSpedCompleto );
end;

procedure TACBrECFBematech.UnLoadDLLFunctions;
var
  sLibName: String;
begin
  sLibName := '';
  // Verifica se exite o caminho das DLLs
  if Length(PathDLL) > 0 then
     sLibName := PathWithDelim(PathDLL);

  // Concatena o caminho se exitir mais o nome da DLL.
  sLibName := sLibName + cLIB_Bema;

  UnLoadLibrary(cLIB_Bema);

  xBematech_FI_ReloadINIFile         := Nil;
  xBematech_FI_AbrePortaSerial       := Nil;
  xBematech_FI_FechaPortaSerial      := Nil;
  xBematech_FI_ArquivoMFDPath        := Nil;
  xBematech_FI_EspelhoMFD            := Nil;
  xBematech_FI_GeraRegistrosCAT52MFD := Nil;
  xBematech_FI_DownloadMFD           := Nil;
  xBematech_FI_DownloadMF            := Nil;
  xBematech_FI_RelatorioSintegraMFD  := Nil;
  xBematech_FI_GeraRegistrosSpedCompleto := Nil;
end;

procedure TACBrECFBematech.AbrePortaSerialDLL(const aPath: String);
Var
  Resp : Integer ;
  aPorta, PathIni, IniFile : String ;

  {$IFDEF MSWINDOWS}
  procedure ConfiguraBemaFI32ini(const aPorta, aPath : String ) ;
  var
    Ini : TIniFile ;
  begin
    GravaLog( '   Verificando arquivo: '+IniFile+', Porta:'+aPorta+', Path:'+aPath );

    Ini := TIniFile.Create( IniFile );
    try
       Ini.WriteString('Sistema','Porta',aPorta ) ;
       Ini.WriteString('Sistema','ControlePorta','1') ;
       if aPath <> '' then
          Ini.WriteString('Sistema','Path',aPath ) ;

       if TACBrECF(fpOwner).Modelo = ecfEscECF then
         Ini.WriteInteger('Sistema','ProtocoloUnico', 1)
       else
         Ini.WriteInteger('Sistema','ProtocoloUnico', 0) ;

    finally
       Ini.Free ;
    end ;
  end;
  {$ENDIF}

begin
  aPorta := fpDevice.Porta;

  GravaLog( '   Desativando ACBrECF' );
  Ativo := False ;
  Sleep( 300 ) ;

  {$IFDEF MSWINDOWS}
   PathIni := ExtractFilePath( PathDLL );
   if PathIni = '' then
     PathIni := ApplicationPath;

   {$IFDEF CPU64}
    IniFile := PathIni+'BemaFi64.INI' ;
   {$ELSE}
    IniFile := PathIni+'BemaFi32.INI' ;
   {$ENDIF}

   if FileExists( IniFile ) then
      ConfiguraBemaFI32ini(aPorta, aPath);
  {$ENDIF}

  LoadDLLFunctions;

  {$IFDEF MSWINDOWS}
   xBematech_FI_ReloadINIFile;
  {$ENDIF}

  GravaLog( '   xBematech_FI_AbrePortaSerial' );
  Resp := xBematech_FI_AbrePortaSerial();
{
  1: OK.
 -4: O arquivo de inicialização BemaFI32.ini não foi encontrado no diretório de sistema do Windows.
 -5: Erro ao abrir a porta de comunicação.
}
  {$IFDEF MSWINDOWS}
   if Resp = -4 then
   begin
      GravaLog( '      Erro = -4' );
      ConfiguraBemaFI32ini( aPorta, aPath ) ;
      GravaLog( '   xBematech_FI_AbrePortaSerial' );
      Resp := xBematech_FI_AbrePortaSerial();
   end ;
  {$ENDIF}

(*
  if Resp = -5 then
  begin
     GravaLog( '      Erro = -5' );
     ConfiguraBemaFI32ini( 'Default', aPath ) ;
     GravaLog( '   xBematech_FI_AbrePortaSerial' );
     Resp := xBematech_FI_AbrePortaSerial();
  end ;
*)

  if Resp <> 1 then
     raise EACBrECFErro.Create( ACBrStr('Erro em Bematech_FI_AbrePortaSerial'+sLineBreak+
                                AnalisarRetornoDll(Resp) ));
end ;

procedure TACBrECFBematech.FechaPortaSerialDLL(const OldAtivo: Boolean);
begin
  GravaLog( '   xBematech_FI_FechaPortaSerial' ) ;
  xBematech_FI_FechaPortaSerial ;

  UnloadDLLFunctions;

  GravaLog( '   Ativar ACBr: '+ifthen(OldAtivo,'SIM','NAO') ) ;
  if OldAtivo then
     Ativo := OldAtivo;
end;

function TACBrECFBematech.AnalisarRetornoDll(const ARetorno: Integer): String;
begin
  case ARetorno of
      0: Result := 'Erro de Comunicação !';
     -1: Result := 'Erro de Execução na Função. Verifique!';
     -2: Result := 'Parâmetro Inválido !';
     -3: Result := 'Alíquota não programada !';
     -4: Result := 'Arquivo BemaFI32.INI não encontrado. Verifique!';
     -5: Result := 'Erro ao Abrir a Porta de Comunicação';
     -6: Result := 'Impressora Desligada ou Desconectada';
     -7: Result := 'Banco Não Cadastrado no Arquivo BemaFI32.ini';
     -8: Result := 'Erro ao Criar ou Gravar no Arquivo Retorno.txt ou Status.txt';
    -18: Result := 'Não foi possível abrir arquivo INTPOS.001 !';
    -19: Result := 'Parâmetro diferentes !';
    -20: Result := 'Transação cancelada pelo Operador !';
    -21: Result := 'A Transação não foi aprovada !';
    -22: Result := 'Não foi possível terminal a Impressão !';
    -23: Result := 'Não foi possível terminal a Operação !';
    -24: Result := 'Forma de pagamento não programada.';
    -25: Result := 'Totalizador não fiscal não programado.';
    -26: Result := 'Transação já Efetuada !';
    -28: Result := 'Não há Informações para serem Impressas !';
  else
    Result := '';
  end;

  Result := 'Cod.: '+IntToStr(ARetorno) + ifthen(Result='','',' - ') + Result;
end;

function TACBrECFBematech.GetProp: AnsiString;
begin
  if Ativo then
  begin
    if (fsProp = '') then
       fsProp := IntToStr( StrToIntDef( UsuarioAtual, 1) ) ;
  end
  else
    fsProp := '1';

  Result := fsProp;
end;

procedure TACBrECFBematech.EspelhoMFD_DLL(DataInicial,
  DataFinal: TDateTime; const NomeArquivo: AnsiString;
  Documentos: TACBrECFTipoDocumentoSet);
var
  Resp : Integer ;
  DiaIni, DiaFim: AnsiString ;
  OldAtivo : Boolean ;
  //{$IFNDEF MSWINDOWS} Cmd, ArqTmp : String ; {$ENDIF}
begin
 //{$IFNDEF MSWINDOWS}
 // ArqTmp := ExtractFilePath( NomeArquivo ) + 'ACBr.mfd' ;
 // SysUtils.DeleteFile( ArqTmp ) ;
 //
 // DiaIni   := FormatDateTime('ddmmyy',DataInicial) ;
 // DiaFim   := FormatDateTime('ddmmyy',DataFinal) ;
 //
 // OldAtivo := Ativo ;
 // try
 //    Ativo := False;
 //
 //    Cmd := fpDevice.Porta + ' ' + ArqTmp+' 1 ' + DiaIni + ' ' + DiaFim + ' ' + Prop;
 //    RunCommand('./linuxmfd',Cmd,True) ;
 //
 //    if not FileExists( ArqTmp ) then
 //       raise EACBrECFErro.Create( ACBrStr('Erro na execução do utilitário "linuxmfd" '+
 //                                       'Arquivo: '+ArqTmp+' não foi criado' ) ) ;
 //
 //    SysUtils.DeleteFile( NomeArquivo ) ;
 //    Cmd := NomeArquivo + ' ' + ArqTmp + ' 1 ' + DiaIni + ' ' + DiaFim + ' ' + Prop ;
 //    RunCommand('./bemamfd2',Cmd,True) ;
 //
 //    if not FileExists( NomeArquivo ) then
 //       raise EACBrECFErro.Create( ACBrStr( 'Erro na execução do utilitário "bemamfd2".'+sLineBreak+
 //                               'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
 // finally
 //    SysUtils.DeleteFile( ArqTmp ) ;
 //    Ativo := OldAtivo ;
 // end;
 //{$ELSE}
  DiaIni   := FormatDateTime('dd"/"mm"/"yyyy', DataInicial) ;
  DiaFim   := FormatDateTime('dd"/"mm"/"yyyy', DataFinal) ;
  OldAtivo := Ativo ;
  try
     SysUtils.DeleteFile(NomeArquivo);
     AbrePortaSerialDLL( ExtractFilePath( NomeArquivo ) ) ;

     Resp := xBematech_FI_EspelhoMFD( NomeArquivo, DiaIni, DiaFim, 'D',
                                      Prop, cChavePublica, cChavePrivada );

     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar Bematech_FI_EspelhoMFD.'+sLineBreak+
                                         AnalisarRetornoDll(Resp) )) ;

     if not FileExists( NomeArquivo ) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro na execução de Bematech_FI_EspelhoMFD.'+sLineBreak+
                                'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
  finally
     FechaPortaSerialDLL( OldAtivo );
  end;
 //{$ENDIF}
end;

procedure TACBrECFBematech.EspelhoMFD_DLL(COOInicial, COOFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
var
  Resp : Integer ;
  CooIni, CooFim: AnsiString ;
  OldAtivo : Boolean ;
  //{$IFNDEF MSWINDOWS} Cmd, ArqTmp : String ; {$ENDIF}
begin
  CooIni := IntToStrZero( COOInicial, 6 ) ;
  CooFim := IntToStrZero( COOFinal, 6 ) ;

 //{$IFNDEF MSWINDOWS}
 // ArqTmp := ExtractFilePath( NomeArquivo ) + 'ACBr.mfd' ;
 // SysUtils.DeleteFile( ArqTmp ) ;
 //
 // OldAtivo := Ativo ;
 // try
 //    Ativo := False;
 //
 //    Cmd := fpDevice.Porta + ' ' + ArqTmp+' 2 '+ CooIni + ' ' + CooFim + ' ' + Prop  ;
 //    RunCommand('./linuxmfd',Cmd,True) ;
 //
 //    if not FileExists( ArqTmp ) then
 //       raise EACBrECFErro.Create( ACBrStr('Erro na execução do utilitário "linuxmfd" '+
 //                                       'Arquivo: '+ArqTmp+' não foi criado' ) ) ;
 //
 //    SysUtils.DeleteFile( NomeArquivo ) ;
 //    Cmd := NomeArquivo + ' ' + ArqTmp + ' 2 ' + CooIni + ' ' + CooFim + ' ' + Prop ;
 //    RunCommand('./bemamfd2',Cmd,True) ;
 //
 //    if not FileExists( NomeArquivo ) then
 //       raise EACBrECFErro.Create( ACBrStr( 'Erro na execução do utilitário "bemamfd2".'+sLineBreak+
 //                               'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
 // finally
 //    SysUtils.DeleteFile( ArqTmp ) ;
 //    Ativo := OldAtivo ;
 // end;
 //{$ELSE}
  OldAtivo := Ativo ;
  try
     SysUtils.DeleteFile(NomeArquivo);
     AbrePortaSerialDLL( ExtractFilePath( NomeArquivo ) ) ;

     Resp := xBematech_FI_EspelhoMFD( NomeArquivo, CooIni, CooFim, 'C',
                                      Prop, cChavePublica, cChavePrivada );

     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar Bematech_FI_EspelhoMFD.'+sLineBreak+
                                         AnalisarRetornoDll(Resp) )) ;

     if not FileExists( NomeArquivo ) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro na execução de Bematech_FI_EspelhoMFD.'+sLineBreak+
                                'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
  finally
     FechaPortaSerialDLL( OldAtivo );
  end;
 //{$ENDIF}
end;

procedure TACBrECFBematech.PafMF_GerarCAT52(const DataInicial,
  DataFinal: TDateTime; const DirArquivos: String; NumeroSerie: String);
var
  Resp: Integer;
  FilePath, DiaIni, DiaFim: AnsiString;
  OldAtivo: Boolean;
  FileMFD: AnsiString;
  DataArquivo: TDateTime;
begin
  DiaIni   := FormatDateTime('dd/mm/yyyy', DataInicial);
  DiaFim   := FormatDateTime('dd/mm/yyyy', DataFinal);

  FilePath := IncludeTrailingPathDelimiter(DirArquivos);
  FileMFD  := AnsiString(FilePath + 'download.mfd');

  OldAtivo := Ativo ;
  try
    // a bematech não possui a geração do CAT52 por período, mas pode-se
    // gerar arquivos de um arquivo MFD, então baixamos a MFD para o periodo
    // e rodamos um loop com a data gerando o arquivo para cada dia dentro
    // do período
    AbrePortaSerialDLL( FilePath ) ;

    // fazer primeiro o download da MFD para o período
    Resp := xBematech_FI_DownloadMFD( FileMFD, '1', DiaIni, DiaFim, Prop );
    if (Resp <> 1) then
    begin
      raise EACBrECFErro.Create(ACBrStr(
        'Erro ao executar xBematech_FI_DownloadMFD.' + sLineBreak +
        AnalisarRetornoDll(Resp)
      ));
    end;

    // gerar o arquivo para cada dia dentro do período a partir da
    // MFD baixada da impressora fiscal
    DataArquivo := DataInicial;
    repeat
      DiaIni := FormatDateTime('dd/mm/yyyy', DataArquivo);

      Resp := xBematech_FI_GeraRegistrosCAT52MFD( FileMFD, DiaIni ) ;
      if (Resp <> 1) then
      begin
        raise EACBrECFErro.Create(ACBrStr(
          'Erro ao executar xBematech_FI_GeraRegistrosCAT52MFD.' + sLineBreak +
          AnalisarRetornoDll(Resp) + sLineBreak +
          'Para a data de: "' + DiaIni + '"'
        ));
      end;

      // próximo dia
      DataArquivo := IncDay( DataArquivo, 1 );

    until DataArquivo > DataFinal;

  finally
    FechaPortaSerialDLL( OldAtivo );
  end;
end;

procedure TACBrECFBematech.ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD;
  const NomeArquivo: AnsiString; StrInicial, StrFinal: AnsiString);
var
  Resp: Integer;
  FilePath, TipoBema: AnsiString;
  OldAtivo: Boolean;
begin
  case Tipo of
    tdmfdData: TipoBema := '1';
    tdmfdCOO:  TipoBema := '2';
  else
    TipoBema := '0';
  end;

  FilePath := ExtractFilePath( NomeArquivo );
  OldAtivo := Ativo ;
  try
     SysUtils.DeleteFile( NomeArquivo );
     AbrePortaSerialDLL( FilePath ) ;

     GravaLog( '   xBematech_FI_DownloadMFD' );
     Resp := xBematech_FI_DownloadMFD( NomeArquivo, TipoBema, StrInicial, StrFinal, Prop ) ;

     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar xBematech_FI_ArquivoMFD.'+sLineBreak+
                                         AnalisarRetornoDll(Resp) )) ;

  finally
     FechaPortaSerialDLL( OldAtivo );
  end;

end;

procedure TACBrECFBematech.ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString);
Var
  Resp : Integer ;
  FilePath : AnsiString ;
  OldAtivo : Boolean ;
begin
  FilePath := ExtractFilePath( NomeArquivo );
  OldAtivo := Ativo ;
  try
     SysUtils.DeleteFile( NomeArquivo );
     AbrePortaSerialDLL( FilePath ) ;

     GravaLog( '   xBematech_FI_DownloadMF' );
     Resp := xBematech_FI_DownloadMF( NomeArquivo ) ;

     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar xBematech_FI_ArquivoMF.'+sLineBreak+
                                         AnalisarRetornoDll(Resp) )) ;

  finally
     FechaPortaSerialDLL( OldAtivo );
  end;
end;

procedure TACBrECFBematech.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
Var
  Resp, Tipo : Integer ;
  DiaIni, DiaFim, Prefixo, FilePath : AnsiString ;
  OldAtivo : Boolean ;
  //{$IFNDEF MSWINDOWS} Cmd, ArqTmp : String ; {$ENDIF}
begin
  FilePath := ExtractFilePath( NomeArquivo );
  Tipo     := 2;
  Prefixo  := 'TDM';

  FinalidadeToTipoPrefixo( Finalidade, Tipo, Prefixo );

 //{$IFNDEF MSWINDOWS}
 // ArqTmp := FilePath +'ACBr.mfd';
 // SysUtils.DeleteFile( ArqTmp ) ;
 //
 // DiaIni := FormatDateTime('ddmmyy',DataInicial) ;
 // DiaFim := FormatDateTime('ddmmyy',DataFinal) ;
 //
 // OldAtivo := Ativo ;
 // try
 //    Ativo := False;
 //
 //    Cmd := fpDevice.Porta + ' ' + ArqTmp+' 3 ' + DiaIni + ' ' + DiaFim + ' ' + Prop;
 //    RunCommand('./linuxmfd',Cmd,True) ;
 //
 //    if not FileExists( ArqTmp ) then
 //       raise EACBrECFErro.Create( ACBrStr('Erro na execução do utilitário "linuxmfd" '+
 //                                       'Arquivo: '+ArqTmp+' não foi criado' ) ) ;
 //
 //    SysUtils.DeleteFile( NomeArquivo ) ;
 //    Cmd := NomeArquivo + ' ' + ArqTmp + ' 3 ' + DiaIni + ' ' + DiaFim + ' ' + Prop ;
 //    RunCommand('./bemamfd2',Cmd,True) ;
 //
 //    if not FileExists( NomeArquivo ) then
 //       raise EACBrECFErro.Create( ACBrStr( 'Erro na execução do utilitário "bemamfd2".'+sLineBreak+
 //                               'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
 // finally
 //    SysUtils.DeleteFile( ArqTmp ) ;
 //    Ativo := OldAtivo ;
 // end;
 //{$ELSE}
  DiaIni   := FormatDateTime('dd"/"mm"/"yyyy', DataInicial) ;
  DiaFim   := FormatDateTime('dd"/"mm"/"yyyy', DataFinal) ;
  OldAtivo := Ativo ;
  try
     SysUtils.DeleteFile( NomeArquivo );

     AbrePortaSerialDLL( FilePath ) ;

     case Finalidade of
       finMF, finMFD, finTDM,
       finRZ, finRFD:
         begin
          Resp := xBematech_FI_ArquivoMFDPath( '',           // Origem = MFD
                                              NomeArquivo,  // Destino
                                              DiaIni, DiaFim, 'D', Prop, Tipo,
                                              cChavePublica, cChavePrivada, 1 ) ;

          if (Resp <> 1) then
             raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar xBematech_FI_ArquivoMFDPath.'+sLineBreak+
                                        AnalisarRetornoDll(Resp) )) ;

          if not FileExists( NomeArquivo ) then
             raise EACBrECFErro.Create( ACBrStr( 'Erro na execução de xBematech_FI_ArquivoMFDPath.'+sLineBreak+
                                        'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
         end;
       //finNFP, finNFPTDM: raise EACBrECFErro.Create( ACBrStr( 'Utilize o método PafMF_GerarCAT52.'));
       finSintegra:
         begin
           if FormatDateTime('mmyyyy', DataInicial) <> FormatDateTime('mmyyyy', DataFinal) then
             raise EACBrECFErro.Create( ACBrStr( 'Permitido somente o período de um mês.'));



           Resp := xBematech_FI_RelatorioSintegraMFD(63, //Gera todos os registros (60M, 60A, 60D, 60I, 60R e 75)
                                                         //Os registros 10, 11 e 90, são gerados automaticamente
                                                    NomeArquivo,
                                                    FormatDateTime('mm', DataInicial),
                                                    FormatDateTime('yyyy', DataInicial),
                                                    'RAZAOSOCIAL', 'ENDERECO', '12345',
                                                    'COMP', 'BAIRRO', 'CIDADE',
                                                    '12345678', '12345678', '12345678',
                                                    'CONTATO');
           if (Resp <> 1) then
             raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar xBematech_FI_RelatorioSintegraMFD.'+sLineBreak+
                                        AnalisarRetornoDll(Resp) )) ;

           if not FileExists( NomeArquivo ) then
             raise EACBrECFErro.Create( ACBrStr( 'Erro na execução de xBematech_FI_RelatorioSintegraMFD.'+sLineBreak+
                                        'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
         end;
       finSPED:
         begin
           Resp := xBematech_FI_GeraRegistrosSpedCompleto('', NomeArquivo,
                                                         DiaIni, DiaFim,
                                                         'T', '5102', ' ',
                                                         '00,00', '00,00',
                                                         'TESTE', '1234567');
           if (Resp <> 1) then
             raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar xBematech_FI_GeraRegistrosSpedCompleto.'+sLineBreak+
                                        AnalisarRetornoDll(Resp) )) ;

           if not FileExists( NomeArquivo ) then
             raise EACBrECFErro.Create( ACBrStr( 'Erro na execução de xBematech_FI_GeraRegistrosSpedCompleto.'+sLineBreak+
                                        'Arquivo: "'+NomeArquivo + '" não gerado' )) ;                                                         
         end;
     end;

  finally
     FechaPortaSerialDLL( OldAtivo );
  end;
 //{$ENDIF}
end;

procedure TACBrECFBematech.ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
  const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD; TipoContador: TACBrECFTipoContador);
Var
  Resp, Tipo : Integer ;
  DadoInicial, DadoFinal, Prefixo, FilePath, TipoDownload : AnsiString ;
  OldAtivo : Boolean ;
  //{$IFNDEF MSWINDOWS} Cmd, ArqTmp : String ; {$ENDIF}
begin
  FilePath := ExtractFilePath( NomeArquivo );
  Tipo     := 2;
  Prefixo  := 'TDM';

  FinalidadeToTipoPrefixo( Finalidade, Tipo, Prefixo );

 //{$IFNDEF MSWINDOWS}
 // if TipoContador = tpcCRZ then
 //    CRZToCOO(ContInicial, ContFinal, ContInicial, ContFinal) ;
 //
 // DadoInicial := IntToStrZero( ContInicial, 6 ) ;
 // DadoFinal   := IntToStrZero( ContFinal, 6 ) ;
 //
 // ArqTmp := FilePath + 'ACBr.mfd';
 // SysUtils.DeleteFile( ArqTmp ) ;
 //
 // OldAtivo := Ativo ;
 // try
 //    Ativo := False;
 //
 //    Cmd := fpDevice.Porta + ' ' + ArqTmp+' 3 ' + DadoInicial + ' ' + DadoFinal + ' ' + Prop;
 //    RunCommand('./linuxmfd',Cmd,True) ;
 //
 //    if not FileExists( ArqTmp ) then
 //       raise EACBrECFErro.Create( ACBrStr('Erro na execução do utilitário "linuxmfd" '+
 //                                       'Arquivo: '+ArqTmp+' não foi criado' ) ) ;
 //
 //    SysUtils.DeleteFile( NomeArquivo ) ;
 //    Cmd := NomeArquivo + ' ' + ArqTmp + ' 3 ' + DadoInicial + ' ' + DadoFinal + ' ' + Prop ;
 //    RunCommand('./bemamfd2',Cmd,True) ;
 //
 //    if not FileExists( NomeArquivo ) then
 //       raise EACBrECFErro.Create( ACBrStr( 'Erro na execução do utilitário "bemamfd2".'+sLineBreak+
 //                               'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
 // finally
 //    SysUtils.DeleteFile( ArqTmp ) ;
 //    Ativo := OldAtivo ;
 // end;
 //{$ELSE}
  if (TipoContador = tpcCRZ) then
  begin
     DadoInicial  := IntToStrZero( ContInicial, 4 ) ;
     DadoFinal    := IntToStrZero( ContFinal, 4 ) ;
     TipoDownload := 'Z';
  end
  else
  begin
    DadoInicial  := IntToStrZero( ContInicial, 6 ) ;
    DadoFinal    := IntToStrZero( ContFinal, 6 ) ;
    TipoDownload := 'C';
  end ;

  OldAtivo := Ativo ;
  try
     SysUtils.DeleteFile( NomeArquivo );

     AbrePortaSerialDLL( FilePath ) ;

     Resp := xBematech_FI_ArquivoMFDPath( '',           // Origem = MFD
                                          NomeArquivo,  // Destino
                                          DadoInicial, DadoFinal, TipoDownload, Prop, Tipo,
                                          cChavePublica, cChavePrivada, 1 ) ;
     if (Resp <> 1) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro ao executar xBematech_FI_ArquivoMFDPath.'+sLineBreak+
                                   AnalisarRetornoDll(Resp) )) ;

     if not FileExists( NomeArquivo ) then
        raise EACBrECFErro.Create( ACBrStr( 'Erro na execução de xBematech_FI_ArquivoMFDPath.'+sLineBreak+
                                   'Arquivo: "'+NomeArquivo + '" não gerado' )) ;
  finally
     FechaPortaSerialDLL( OldAtivo );
  end;
 //{$ENDIF}
end;

procedure TACBrECFBematech.CRZToCOO(const ACRZIni, ACRZFim: Integer;
  var ACOOIni, ACOOFim: Integer);
var
  Retorno: TStringList;
  CRZi, CRZf: string;
  Linha: string;
  I, PosCOO: Integer;
begin
  Retorno := TStringList.Create;
  try
    Self.LeituraMemoriaFiscalSerial(ACRZIni, ACRZFim, Retorno);

    Retorno.Text := Trim(Retorno.Text);
    //Retorno.SaveToFile('c:\temp\retorno.txt');

    if Retorno.Text = '' then
      raise EACBrECFErro.Create( 'Nenhuma leitura serial não foi retornada pela impressora fiscal.');

    PosCOO  := 0;
    ACOOIni := 0;
    ACOOFim := 0;
    CRZi    := Format('%4.4d  ', [ACRZIni]);
    CRZf    := Format('%4.4d  ', [ACRZFim]);

    I := 0 ;
    while (I < Retorno.Count) and ( (ACOOIni = 0) or (ACOOFim = 0) ) do
    begin
      Linha := Retorno[I];

      if PosCOO = 0 then  // Já achou o cabeçalho ?
      begin
        if (pos('CRZ ',Linha) = 1) then
          PosCOO := pos('COO ', Linha);
      end
      else
      begin
        // TODO: Verificar menor e maior COO das RZ especificadas ??
        if Copy(Linha, 1, 6) = CRZi then
          ACOOIni := StrToIntDef(Copy(Linha, PosCOO, 6), 0);

        if Copy(Linha, 1, 6) = CRZf then
          ACOOFim := StrToIntDef(Copy(Linha, PosCOO, 6), 0);
      end ;

      Inc( I ) ;
    end ;

    if ACOOIni = 0 then
       raise EACBrECFErro.Create( ACBrStr('Periodo inicial - CRZ: '+CRZi+' não encontrado') );

    if ACOOFim = 0 then
       raise EACBrECFErro.Create( ACBrStr('Periodo final - CRZ: '+CRZf+' não encontrado') );

    GravaLog('CRZ Inicial: '+IntToStr(ACRZIni)+' - COO: '+IntToStr(ACOOIni) +sLineBreak +
             'CRZ Final: '+IntToStr(ACRZFim)+' - COO: '+IntToStr(ACOOFim) );
  finally
    Retorno.Free;
  end;
end;

procedure TACBrECFBematech.FinalidadeToTipoPrefixo(
   AFinalidade: TACBrECFFinalizaArqMFD; var Tipo: Integer; var Prefixo: AnsiString);
begin
  Tipo    := 2;
  Prefixo := 'TDM';

  case AFinalidade of
    finMF:
      begin
        Prefixo := 'MF' ;
        Tipo    := 0;
      end;
    finMFD:
      begin
        Prefixo := 'MFD' ;
        Tipo    := 1;
      end;
    finRZ:
      begin
        Prefixo := 'RZ' ;
        Tipo    := 3;
      end;
    finRFD:
      begin
        Prefixo := 'RFD' ;
        Tipo    := 4;
      end;
  end;
end;

function TACBrECFBematech.ACKValido(const nACK: Integer): Boolean;
begin
  Result := (nACK = 6) or (nACK = 255);
end;

function TACBrECFBematech.TraduzirTag(const ATag: AnsiString): AnsiString;
begin
   Result := BematechTraduzirTag( ATag );
end;

function TACBrECFBematech.TraduzirTagBloco(const ATag, Conteudo : AnsiString
   ) : AnsiString ;
begin
  Result := BematechTraduzirTagBloco( ATag, Conteudo, Self);
end ;

end.


