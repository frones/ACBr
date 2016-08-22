////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
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

unit pcnCCeNFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador;

type
  TDetEvento               = class;
  TInfEvento               = class;
  TInfEventoCollection     = class;
  TInfEventoCollectionItem = class;
  TCCeNFe                  = class;

  TDetEvento = class
  private
    FVersao: String;
    FDescEvento: String;
    FCorrecao: String;
    FCondUso: String;

    procedure setCondUso(const Value: String);
  public
    property versao: String     read FVersao     write FVersao;
    property descEvento: String read FDescEvento write FDescEvento;
    property xCorrecao: String  read FCorrecao   write FCorrecao;
    property xCondUso: String   read FCondUso    write setCondUso;
  end;

  TInfEvento = class
    FID: String;
    FOrgao: Integer;
    FtpAmbiente: TpcnTipoAmbiente;
    FCNPJ: String;
    FChave: String;
    FDataEvento: TDateTime;
    FTpEvento: Integer;
    FnSeqEvento: Integer;
    FVersaoEvento: String;
    FDetEvento: TDetEvento;
  public
    constructor Create;
    destructor Destroy; override;
    property id: String              read FID           write FID;
    property cOrgao: Integer         read FOrgao        write FOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente   write FtpAmbiente;
    property CNPJ: String            read FCNPJ         write FCNPJ;
    property chNFe: String           read FChave        write FChave;
    property dhEvento: TDateTime     read FDataEvento   write FDataEvento;
    property tpEvento: Integer       read FTpEvento     write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento   write FnSeqEvento;
    property versaoEvento: String    read FVersaoEvento write FVersaoEvento;
    property detEvento: TDetEvento   read FDetEvento    write FDetEvento;
  end;

  TInfEventoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TInfEventoCollectionItem = class(TCollectionItem)
  private
    FInfEvento: TInfEvento;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property InfEvento: TInfEvento read FInfEvento write FInfEvento;
  end;

  TCCeNFe = class(TPersistent)
  private
    FGerador: TGerador;
    FidLote: Integer;
    FEvento: TInfEventoCollection;
    FVersao: String;

    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;
    function ObterNomeArquivo: String;
  published
    property Gerador: TGerador             read FGerador     write FGerador;
    property idLote: Integer               read FidLote      write FidLote;
    property Evento: TInfEventoCollection  read FEvento      write SetEvento;
    property Versao: String                read FVersao      write FVersao;
  end;

implementation

Uses pcnAuxiliar,
  ACBrUtil;

{ TCCeNFe }

constructor TCCeNFe.Create;
begin
  FGerador   := TGerador.Create;
  FEvento    := TInfEventoCollection.Create(Self);
end;

destructor TCCeNFe.Destroy;
begin
  FGerador.Free;
  FEvento.Free;
  inherited;
end;

function TCCeNFe.ObterNomeArquivo: String;
begin
  Result := IntToStr(Self.idLote) + '-cce.xml';
end;

function TCCeNFe.GerarXML: boolean;
var
 i : integer;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('envEvento ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcInt, 'HP03', 'idLote', 001, 015, 1, FidLote, DSC_IDLOTE);
  for i:= 0 to Evento.Count - 1 do
   begin
     Evento.Items[i].InfEvento.id := 'ID110110' + OnlyNumber(Evento.Items[i].InfEvento.chNFe) + Format('%.2d', [Evento.Items[i].InfEvento.nSeqEvento]);

     Gerador.wGrupo('evento ' + NAME_SPACE + ' versao="' + Versao + '"');
     Gerador.wGrupo('infEvento Id="' + Evento.Items[i].InfEvento.id + '"');
     if Length(Evento.Items[i].InfEvento.id) < 54 then
       Gerador.wAlerta('HP07', 'ID', '', 'ID de carta de correção inválido');
     Gerador.wCampo(tcInt, 'HP08', 'cOrgao', 001, 002, 1, Evento.Items[i].InfEvento.cOrgao);
     Gerador.wCampo(tcStr, 'HP09', 'tpAmb', 001, 001,  1, TpAmbToStr(Evento.Items[i].InfEvento.tpAmb), DSC_TPAMB);
     if Length(OnlyNumber(Evento.Items[i].InfEvento.CNPJ)) = 14 then
      begin
        Gerador.wCampo(tcStr, 'HP10', 'CNPJ', 014, 014, 1, OnlyNumber(Evento.Items[i].InfEvento.CNPJ), DSC_CNPJ);
        if not ValidarCNPJ(OnlyNumber(Evento.Items[i].InfEvento.CNPJ)) then
          Gerador.wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
      end
     else if Length(OnlyNumber(Evento.Items[i].InfEvento.CNPJ)) = 11 then
      begin
        Gerador.wCampo(tcStr, 'HP11', 'CPF', 011, 011, 1, OnlyNumber(Evento.Items[i].InfEvento.CNPJ), DSC_CPF);
        if not ValidarCPF(OnlyNumber(Evento.Items[i].InfEvento.CNPJ)) then
          Gerador.wAlerta('HP11', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
      end;
     Gerador.wCampo(tcStr,    'HP12', 'chNFe', 044, 044,      1, Evento.Items[i].InfEvento.chNFe, DSC_CHAVE);
     if not ValidarChave(Evento.Items[i].InfEvento.chNFe) then
       Gerador.wAlerta('HP12', 'chNFe', '', 'Chave de NFe inválida');
     Gerador.wCampo(tcStr,    'HP13', 'dhEvento', 001, 050,   1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Evento.Items[i].InfEvento.dhEvento)+
                                                                 GetUTC(CodigoParaUF(Evento.Items[i].InfEvento.cOrgao), Evento.Items[i].InfEvento.dhEvento));
     Gerador.wCampo(tcInt,    'HP14', 'tpEvento', 006, 006,   1, Evento.Items[i].InfEvento.tpEvento);
     Gerador.wCampo(tcInt,    'HP15', 'nSeqEvento', 001, 002, 1, Evento.Items[i].InfEvento.nSeqEvento);
     Gerador.wCampo(tcStr,    'HP16', 'verEvento', 001, 004,  1, Evento.Items[i].InfEvento.versaoEvento);
     Gerador.wGrupo('detEvento versao="' +  Versao + '"');
     Gerador.wCampo(tcStr,    'HP19', 'descEvento', 005, 060, 1,  Evento.Items[i].InfEvento.detEvento.descEvento);
     Gerador.wCampo(tcStr,    'HP20', 'xCorrecao', 015, 1000, 1,  Evento.Items[i].InfEvento.detEvento.xCorrecao);
     Gerador.wCampo(tcStr,    'HP20a', 'xCondUso', 001, 5000, 1,  Evento.Items[i].InfEvento.detEvento.xCondUso);
     Gerador.wGrupo('/detEvento');
     Gerador.wGrupo('/infEvento');
     Gerador.wGrupo('/evento');
  end;
  Gerador.wGrupo('/envEvento');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TCCeNFe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

{ TInfEvento }

constructor TInfEvento.Create;
begin
  inherited Create;
  FDetEvento := TDetEvento.Create;
end;

destructor TInfEvento.Destroy;
begin
  FDetEvento.Free;
  inherited;
end;

{ TDetEvento }

procedure TDetEvento.setCondUso(const Value: String);
begin
  FCondUso := Value;

  if FCondUso = '' then
    FCondUso := 'A Carta de Correcao e disciplinada pelo paragrafo 1o-A do' +
                ' art. 7o do Convenio S/N, de 15 de dezembro de 1970 e' +
                ' pode ser utilizada para regularizacao de erro ocorrido na' +
                ' emissao de documento fiscal, desde que o erro nao esteja' +
                ' relacionado com: I - as variaveis que determinam o valor' +
                ' do imposto tais como: base de calculo, aliquota, diferenca' +
                ' de preco, quantidade, valor da operacao ou da prestacao;' +
                ' II - a correcao de dados cadastrais que implique mudanca' +
                ' do remetente ou do destinatario; III - a data de emissao ou' +
                ' de saida.'
end;

{ TInfEventoCollection }

function TInfEventoCollection.Add: TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfEventoCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TInfEventoCollectionItem);
end;

function TInfEventoCollection.GetItem(
  Index: Integer): TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited GetItem(Index));
end;

procedure TInfEventoCollection.SetItem(Index: Integer;
  Value: TInfEventoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
  FInfEvento := TInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  inherited;
end;

end.
