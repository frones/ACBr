////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar CTe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da CTe          //
//                                                                            //
//        site: www.projetocooperar.org/cte                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_cte/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcteEventoCTe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnConversao, pcteConversaoCTe;

type
  TInfEvento      = class;
  TDetEvento      = class;
  TRetInfEvento   = class;
  EventoException = class(Exception);

  TInfCorrecaoCollection  = class;
  TInfCorrecaoCollectionItem = class;

  TInfGTVCollection  = class;
  TInfGTVCollectionItem = class;
  TInfEspecieCollection  = class;
  TInfEspecieCollectionItem = class;

  TInfRemDest = class;

  TInfEvento = class
  private
    FId: String;
    FtpAmbiente: TpcnTipoAmbiente;
    FCNPJ: String;
    FcOrgao: Integer;
    FChave: String;
    FDataEvento: TDateTime;
    FTpEvento: TpcnTpEvento;
    FnSeqEvento: Integer;
    FVersaoEvento: String;
    FDetEvento: TDetEvento;

    function getcOrgao: Integer;
    function getDescEvento: String;
    function getTipoEvento: String;
  public
    constructor Create;
    destructor Destroy; override;
    function DescricaoTipoEvento(TipoEvento:TpcnTpEvento): String;

    property Id: String              read FId             write FId;
    property cOrgao: Integer         read getcOrgao       write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente     write FtpAmbiente;
    property CNPJ: String            read FCNPJ           write FCNPJ;
    property chCTe: String           read FChave          write FChave;
    property dhEvento: TDateTime     read FDataEvento     write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento       write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
    property versaoEvento: String    read FVersaoEvento   write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento      write FDetEvento;
    property DescEvento: String      read getDescEvento;
    property TipoEvento: String      read getTipoEvento;
  end;

  TDetEvento = class(TPersistent)
  private
    FdescEvento: String;
    FnProt: String;

    FxJust: String;    // Cancelamento
    FxOBS: String;    // Cancelamento

    FvICMS: Currency;  // EPEC
    FvTPrest: Currency;
    FvCarga: Currency;
    Ftoma: TpcteTomador;
    FUF: String;
    FCNPJCPF: String;
    FIE: String;
    Fmodal: TpcteModal;
    FUFIni: String;
    FUFFim: String;
    FtpCTe: TpcteTipoCTe;
    FdhEmi: TDateTime;

    FxRegistro: String; // MultiModal
    FnDoc: String;

    FinfCorrecao: TInfCorrecaoCollection;
    FCondUso: String;
     // GTV
    FinfGTV: TInfGTVCollection;

    procedure SetCorrecao(const Value: TInfCorrecaoCollection);
    procedure setCondUso(const Value: String);
    procedure SetGTV(const Value: TInfGTVCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property descEvento: String read FdescEvento write FdescEvento;
    property nProt: String      read FnProt      write FnProt;

    property xJust: String      read FxJust      write FxJust;
    property xOBS: String       read FxOBS       write FxOBS;

    property vICMS: Currency     read FvICMS      write FvICMS;
    property vTPrest: Currency   read FvTPrest    write FvTPrest;
    property vCarga: Currency    read FvCarga     write FvCarga;
    property toma: TpcteTomador  read Ftoma       write Ftoma;
    property UF: String          read FUF         write FUF;
    property CNPJCPF: String     read FCNPJCPF    write FCNPJCPF;
    property IE: String          read FIE         write FIE;
    property modal: TpcteModal   read Fmodal      write Fmodal;
    property UFIni: String       read FUFIni      write FUFIni;
    property UFFim: String       read FUFFim      write FUFFim;
    property tpCTe: TpcteTipoCTe read FtpCTe      write FtpCTe;
    property dhEmi: TDateTime    read FdhEmi      write FdhEmi;

    property xRegistro: String  read FxRegistro  write FxRegistro;
    property nDoc: String       read FnDoc       write FnDoc;

    property infCorrecao: TInfCorrecaoCollection read FinfCorrecao write SetCorrecao;
    property xCondUso: String                    read FCondUso     write setCondUso;

    property infGTV: TInfGTVCollection read FinfGTV write SetGTV;
  end;

  TInfCorrecaoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfCorrecaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfCorrecaoCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInfCorrecaoCollectionItem;
    property Items[Index: Integer]: TInfCorrecaoCollectionItem read GetItem write SetItem; default;
  end;

  TInfCorrecaoCollectionItem = class(TCollectionItem)
  private
    FgrupoAlterado: String;
    FcampoAlterado: String;
    FvalorAlterado: String;
    FnroItemAlterado: Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property grupoAlterado: String    read FgrupoAlterado   write FgrupoAlterado;
    property campoAlterado: String    read FcampoAlterado   write FcampoAlterado;
    property valorAlterado: String    read FvalorAlterado   write FvalorAlterado;
    property nroItemAlterado: Integer read FnroItemAlterado write FnroItemAlterado;
  end;

  TInfGTVCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfGTVCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfGTVCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInfGTVCollectionItem;
    property Items[Index: Integer]: TInfGTVCollectionItem read GetItem write SetItem; default;
  end;

  TInfGTVCollectionItem = class(TCollectionItem)
  private
    FnDoc: String;
    Fid: String;
    Fserie: String;
    Fsubserie: String;
    FdEmi: TDateTime;
    FnDV: Integer;
    FqCarga: Currency;
    FinfEspecie: TInfEspecieCollection;
    Frem: TInfRemDest;
    Fdest: TInfRemDest;
    Fplaca: String;
    FUF: String;
    FRNTRC: String;

    procedure SetEspecie(const Value: TInfEspecieCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property nDoc: String     read FnDoc     write FnDoc;
    property id: String       read Fid       write Fid;
    property serie: String    read Fserie    write Fserie;
    property subserie: String read Fsubserie write Fsubserie;
    property dEmi: TDateTime  read FdEmi     write FdEmi;
    property nDV: Integer     read FnDV      write FnDV;
    property qCarga: Currency read FqCarga   write FqCarga;
    property infEspecie: TInfEspecieCollection read FinfEspecie write SetEspecie;
    property rem: TInfRemDest  read Frem      write Frem;
    property dest: TInfRemDest read Fdest     write Fdest;
    property placa: String     read Fplaca    write Fplaca;
    property UF: String        read FUF       write FUF;
    property RNTRC: String     read FRNTRC    write FRNTRC;
  end;

  TInfEspecieCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfEspecieCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEspecieCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInfEspecieCollectionItem;
    property Items[Index: Integer]: TInfEspecieCollectionItem read GetItem write SetItem; default;
  end;

  TInfEspecieCollectionItem = class(TCollectionItem)
  private
    FtpEspecie: TEspecie;
    FvEspecie: Currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property tpEspecie: TEspecie read FtpEspecie write FtpEspecie;
    property vEspecie: Currency  read FvEspecie  write FvEspecie;
  end;

  TInfRemDest = class(TPersistent)
  private
    FCNPJCPF: String;
    FIE: String;
    FUF: String;
    FxNome: String;
  published
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property IE: String      read FIE      write FIE;
    property UF: String      read FUF      write FUF;
    property xNome: String   read FxNome   write FxNome;
  end;

  { TRetInfEvento }

  TRetInfEvento = class
  private
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: String;
    FchCTe: String;
    FtpEvento: TpcnTpEvento;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FdhRegEvento: TDateTime;
    FnProt: String;
    FXML: AnsiString;
    FNomeArquivo: String;
  public
  published
    property Id: String              read FId          write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: String        read FverAplic    write FverAplic;
    property cOrgao: Integer         read FcOrgao      write FcOrgao;
    property cStat: Integer          read FcStat       write FcStat;
    property xMotivo: String         read FxMotivo     write FxMotivo;
    property chCTe: String           read FchCTe       write FchCTe;
    property tpEvento: TpcnTpEvento  read FtpEvento    write FtpEvento;
    property xEvento: String         read FxEvento     write FxEvento;
    property nSeqEvento: Integer     read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String        read FCNPJDest    write FCNPJDest;
    property emailDest: String       read FemailDest   write FemailDest;
    property dhRegEvento: TDateTime  read FdhRegEvento write FdhRegEvento;
    property nProt: String           read FnProt       write FnProt;
    property XML: AnsiString         read FXML         write FXML;
    property NomeArquivo: String     read FNomeArquivo write FNomeArquivo;
  end;

implementation

uses
  ACBrUtil;

{ TInfEvento }

constructor TInfEvento.Create;
begin
  inherited Create;
  FDetEvento  := TDetEvento.Create;
  FnSeqEvento := 0;
end;

destructor TInfEvento.Destroy;
begin
  FDetEvento.Free;
  inherited;
end;

function TInfEvento.getcOrgao: Integer;
//  (AC,AL,AP,AM,BA,CE,DF,ES,GO,MA,MT,MS,MG,PA,PB,PR,PE,PI,RJ,RN,RS,RO,RR,SC,SP,SE,TO);
//  (12,27,16,13,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,14,42,35,28,17);
begin
  Result := 0;

  if FTpEvento = teEPEC then
  begin
    case StrToIntDef(copy(FChave, 1, 2), 0) of
      0,
     11, // Rondônia
     12, // Acre
     13, // Amazonas
     14, // Roraima
     15, // Pará
     16, // Amapá
     17, // Tocantins
     21, // Maranhão
     22, // Piauí
     23, // Ceará
     24, // Rio Grande do Norte
     25, // Paraibá
     27, // Alagoas
     28, // Sergipe
     29, // Bahia
     31, // Minas Gerais
     32, // Espirito Santo
     33, // Rio de Janeiro
     41, // Paraná
     42, // Santa Catarina
     43, // Rio Grande do Sul
     52, // Goiás
     53: // Distrito Federal
        Result := 35;
     26, // Pernanbuco
     35, // São Paulo
     50, // Mato Grosso do Sul
     51: // Mato Grosso
        Result := 43;
    end;
  end
  else begin
   if FcOrgao <> 0 then
     Result := FcOrgao
   else
     Result := StrToIntDef(copy(FChave, 1, 2), 0);

   if Result = 0 then
     raise EventoException.Create('Campo cOrgao não informado.');
  end;
end;

function TInfEvento.getDescEvento: String;
var
  Desc: String;
begin
  case FTpEvento of
    teCCe                         : Desc := 'Carta de Correcao';
    teCancelamento                : Desc := 'Cancelamento';
    teManifDestConfirmacao        : Desc := 'Confirmacao da Operacao';
    teManifDestCiencia            : Desc := 'Ciencia da Operacao';
    teManifDestDesconhecimento    : Desc := 'Desconhecimento da Operacao';
    teManifDestOperNaoRealizada   : Desc := 'Operacao nao Realizada';
    teEPECNFe                     : Desc := 'EPEC';
    teEPEC                        : Desc := 'EPEC';
    teMultiModal                  : Desc := 'Registro Multimodal';
    teRegistroPassagem            : Desc := 'Registro de Passagem';
    teRegistroPassagemBRId        : Desc := 'Registro de Passagem BRId';
    teEncerramento                : Desc := 'Encerramento';
    teInclusaoCondutor            : Desc := 'Inclusao Condutor';
    teRegistroCTe                 : Desc := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado: Desc := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID     : Desc := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado               : Desc := 'CT-e Autorizado';
    teCTeCancelado                : Desc := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2             : Desc := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2              : Desc := 'MDF-e Cancelado';
    teVistoriaSuframa             : Desc := 'Vistoria SUFRAMA';
    teConfInternalizacao          : Desc := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    tePrestDesacordo              : Desc := 'Prestacao do Servico em Desacordo';
    teGTV                         : Desc := 'Informacoes da GTV';
    teAutCteComplementar          : Desc := 'Autorizado CTe Complemetnar';
  else
    raise EventoException.Create('Descrição do Evento não Implementado!');
  end;

  Result := ACBrStr(Desc);
end;

function TInfEvento.getTipoEvento: String;
begin
  try
    Result := TpEventoToStr( FTpEvento );
  except
    raise EventoException.Create('Tipo do Evento não Implementado!');
  end;
end;

function TInfEvento.DescricaoTipoEvento(TipoEvento: TpcnTpEvento): String;
begin
  case TipoEvento of
    teCCe                      : Result := 'CARTA DE CORREÇÃO ELETRÔNICA';
    teCancelamento             : Result := 'CANCELAMENTO DO CT-e';
    teManifDestConfirmacao     : Result := 'CONFIRMAÇÃO DA OPERAÇÃO';
    teManifDestCiencia         : Result := 'CIÊNCIA DA OPERAÇÃO';
    teManifDestDesconhecimento : Result := 'DESCONHECIMENTO DA OPERAÇÃO';
    teManifDestOperNaoRealizada: Result := 'OPERAÇÃO NÃO REALIZADA';
    teEPECNFe                  : Result := 'EPEC';
    teEPEC                     : Result := 'EPEC';
    teMultiModal               : Result := 'REGISTRO MULTIMODAL';
    teRegistroPassagem         : Result := 'REGISTRO DE PASSAGEM';
    teRegistroPassagemBRId     : Result := 'REGISTRO DE PASSAGEM BRId';
    teEncerramento             : Result := 'ENCERRAMENTO';
    teInclusaoCondutor         : Result := 'INCLUSAO CONDUTOR';
    teRegistroCTe              : Result := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado: Result := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID     : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado               : Result := 'CT-e Autorizado';
    teCTeCancelado                : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2             : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2              : Result := 'MDF-e Cancelado';
    teVistoriaSuframa             : Result := 'Vistoria SUFRAMA';
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    tePrestDesacordo           : Result := 'Prestação do Serviço em Desacordo';
    teGTV                      : Result := 'Informações da GTV';
    teAutCteComplementar       : Result := 'Autorizado CTe Complementar';
  else
    Result := 'Não Definido';
  end;
end;

{ TInfCorrecaoCollection }

function TInfCorrecaoCollection.Add: TInfCorrecaoCollectionItem;
begin
  Result := TInfCorrecaoCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfCorrecaoCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TInfCorrecaoCollectionItem);
end;

function TInfCorrecaoCollection.GetItem(
  Index: Integer): TInfCorrecaoCollectionItem;
begin
  Result := TInfCorrecaoCollectionItem(inherited GetItem(Index));
end;

procedure TInfCorrecaoCollection.SetItem(Index: Integer;
  Value: TInfCorrecaoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfCorrecaoCollectionItem }

constructor TInfCorrecaoCollectionItem.Create;
begin

end;

destructor TInfCorrecaoCollectionItem.Destroy;
begin

  inherited;
end;

{ TDetEvento }

constructor TDetEvento.Create;
begin
  FinfCorrecao := TInfCorrecaoCollection.Create(Self);
  FinfGTV      := TInfGTVCollection.Create(Self);
end;

destructor TDetEvento.Destroy;
begin
  FInfCorrecao.Free;
  FinfGTV.Free;
  inherited;
end;

procedure TDetEvento.setCondUso(const Value: String);
begin
  FCondUso := 'A Carta de Correcao e disciplinada pelo Art. 58-B do CONVENIO/' +
              'SINIEF 06/89: Fica permitida a utilizacao de carta de correcao,' +
              ' para regularizacao de erro ocorrido na emissao de documentos ' +
              'fiscais relativos a prestacao de servico de transporte, desde ' +
              'que o erro nao esteja relacionado com: I - as variaveis que ' +
              'determinam o valor do imposto tais como: base de calculo, ' +
              'aliquota, diferenca de preco, quantidade, valor da prestacao;' +
              'II - a correcao de dados cadastrais que implique mudanca do ' +
              'emitente, tomador, remetente ou do destinatario;III - a data ' +
              'de emissao ou de saida.';
end;

procedure TDetEvento.SetCorrecao(const Value: TInfCorrecaoCollection);
begin
  FInfCorrecao.Assign(Value);
end;

procedure TDetEvento.SetGTV(const Value: TInfGTVCollection);
begin
  FinfGTV := Value;
end;

{ TInfGTVCollection }

function TInfGTVCollection.Add: TInfGTVCollectionItem;
begin
  Result := TInfGTVCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfGTVCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TInfGTVCollectionItem);
end;

function TInfGTVCollection.GetItem(Index: Integer): TInfGTVCollectionItem;
begin
  Result := TInfGTVCollectionItem(inherited GetItem(Index));
end;

procedure TInfGTVCollection.SetItem(Index: Integer;
  Value: TInfGTVCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfGTVCollectionItem }

constructor TInfGTVCollectionItem.Create;
begin
  FinfEspecie := TInfEspecieCollection.Create(Self);
  Frem := TInfRemDest.Create;
  Fdest := TInfRemDest.Create;
end;

destructor TInfGTVCollectionItem.Destroy;
begin
  FinfEspecie.Free;
  Frem.Free;
  Fdest.Free;
  inherited;
end;

procedure TInfGTVCollectionItem.SetEspecie(
  const Value: TInfEspecieCollection);
begin
  FinfEspecie := Value;
end;

{ TInfEspecieCollection }

function TInfEspecieCollection.Add: TInfEspecieCollectionItem;
begin
  Result := TInfEspecieCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfEspecieCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TInfEspecieCollectionItem);
end;

function TInfEspecieCollection.GetItem(
  Index: Integer): TInfEspecieCollectionItem;
begin
  Result := TInfEspecieCollectionItem(inherited GetItem(Index));
end;

procedure TInfEspecieCollection.SetItem(Index: Integer;
  Value: TInfEspecieCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfEspecieCollectionItem }

constructor TInfEspecieCollectionItem.Create;
begin

end;

destructor TInfEspecieCollectionItem.Destroy;
begin

  inherited;
end;

end.
