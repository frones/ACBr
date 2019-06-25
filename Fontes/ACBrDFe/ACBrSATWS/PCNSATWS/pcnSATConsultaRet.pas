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

unit pcnSATConsultaRet;

interface

uses
  SysUtils, Classes, Contnrs, pcnConversao, pcnLeitor, pcnSignature, ACBrUtil;

type
  TSATConsultaRet = class;
  TLoteCollectionItem   = class;
  TInfCFeCollectionItem = class;

  { TInfCFeCollection }
  TInfCFeCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TInfCFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfCFeCollectionItem);
  public
    function Add: TInfCFeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfCFeCollectionItem;
    property Items[Index: Integer]: TInfCFeCollectionItem read GetItem write SetItem; default;
  end;

  { TInfCFeCollectionItem }
  TInfCFeCollectionItem = class(TObject)
  private
    FChave : String;
    FnCupom : String;
    FSituacao : String;
    FErros : String;
  public
    property Chave    : String        read FChave    write FChave;
    property nCupom   : String        read FnCupom   write FnCupom;
    property Situacao : String        read FSituacao write FSituacao;
    property Erros    : String        read FErros    write FErros;
  end;

  { TLoteCollection }
  TLoteCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TLoteCollectionItem;
    procedure SetItem(Index: Integer; Value: TLoteCollectionItem);
  public
    function Add: TLoteCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TLoteCollectionItem;
    property Items[Index: Integer]: TLoteCollectionItem read GetItem write SetItem; default;
  end;

  { TLoteCollectionItem }
  TLoteCollectionItem = class(TObject)
  private
    FNRec            : String;
    FdhEnvioLote     : TDateTime;
    FdhProcessamento : TDateTime;
    FTipoLote        : String;
    FOrigem          : String;
    FQtdeCupoms      : Integer;
    FSituacaoLote    : String;
    FInfCFe          : TInfCFeCollection;
    FSignature       : TSignature;

    procedure SetInfCFe(AValue: TInfCFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NRec            : String      read FNRec            write FNRec;
    property dhEnvioLote     : TDateTime   read FdhEnvioLote     write FdhEnvioLote;
    property dhProcessamento : TDateTime   read FdhProcessamento write FdhProcessamento;
    property TipoLote        : String      read FTipoLote        write FTipoLote;
    property Origem          : String      read FOrigem          write FOrigem;
    property QtdeCupoms      : Integer     read FQtdeCupoms      write FQtdeCupoms;
    property SituacaoLote    : String      read FSituacaoLote    write FSituacaoLote;
    property InfCFe: TInfCFeCollection     read FInfCFe          write SetInfCFe;
    property Signature       : TSignature  read FSignature       write FSignature;
  end;


  { TSATConsultaRet }

  TSATConsultaRet = class(TObject)
  private
    FLeitor: TLeitor;
    FCNPJ  : String;
    FxNome : String;
    FMensagem : String;
    FLote: TLoteCollection;

    procedure SetLote(const Value: TLoteCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property Leitor   : TLeitor         read FLeitor   write FLeitor;
    property CNPJ     : String          read FCNPJ     write FCNPJ;
    property xNome    : String          read FxNome    write FxNome;
    property Mensagem : String          read FMensagem write FMensagem;
    property Lote     : TLoteCollection read FLote     write SetLote;
  end;

implementation

{ TLoteCollectionItem }

procedure TLoteCollectionItem.SetInfCFe(AValue: TInfCFeCollection);
begin
  FInfCFe.Assign(AValue);
end;

constructor TLoteCollectionItem.Create;
begin
  inherited;

  FInfCFe    := TInfCFeCollection.Create;
  FSignature := TSignature.Create;
end;

destructor TLoteCollectionItem.Destroy;
begin
  FSignature.Free;
  FInfCFe.Free;
  inherited Destroy;
end;

{ TInfCFeCollection }

function TInfCFeCollection.GetItem(Index: Integer): TInfCFeCollectionItem;
begin
  Result := TInfCFeCollectionItem(inherited GetItem(Index));
end;

procedure TInfCFeCollection.SetItem(Index: Integer; Value: TInfCFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TInfCFeCollection.Add: TInfCFeCollectionItem;
begin
  Result := Self.New;
end;

function TInfCFeCollection.New: TInfCFeCollectionItem;
begin
  Result := TInfCFeCollectionItem.Create();
  Self.Add(Result);
end;

{ TLoteCollection }

function TLoteCollection.GetItem(Index: Integer): TLoteCollectionItem;
begin
  Result := TLoteCollectionItem(inherited GetItem(Index));
end;

procedure TLoteCollection.SetItem(Index: Integer; Value: TLoteCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TLoteCollection.Add: TLoteCollectionItem;
begin
  Result := Self.New;
end;

function TLoteCollection.New: TLoteCollectionItem;
begin
  Result := TLoteCollectionItem.Create;
  Self.Add(Result);
end;

{ TSATConsultaRet }

procedure TSATConsultaRet.SetLote(const Value: TLoteCollection);
begin
  FLote.Assign(Value);
end;

constructor TSATConsultaRet.Create;
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FLote   := TLoteCollection.Create;
end;

destructor TSATConsultaRet.Destroy;
begin
  FLote.Free;
  FLeitor.Free;
  inherited;
end;

function TSATConsultaRet.LerXml: Boolean;
var
  i, j: Integer;
begin
  i := 0;
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    Mensagem := trim(Leitor.rCampo(tcStr, 'Mensagem'));

    if Leitor.rExtrai(1, 'infContribuinte') <> '' then
    begin
      CNPJ := trim(Leitor.rCampo(tcStr, 'CNPJ'));
      if (CNPJ <> '') and (length(CNPJ) < 14) then
        CNPJ := PadLeft(CNPJ, 14, '0');

      xNome := trim(Leitor.rCampo(tcStr, 'xNome'));
    end;

    if Leitor.rExtrai(1, 'Lote') <> '' then
    begin
      while Leitor.rExtrai(1, 'Lote', '', i + 1) <> '' do
      begin

        FLote.New;
        FLote[i].NRec            := Leitor.rCampo(tcStr, 'NRec');
        FLote[i].dhEnvioLote     := Leitor.rCampo(tcDatHorCFe, 'dhEnvioLote');
        FLote[i].dhProcessamento := Leitor.rCampo(tcDatHorCFe, 'dhProcessamento');
        FLote[i].TipoLote        := Leitor.rCampo(tcStr, 'TipoLote');
        FLote[i].Origem          := Leitor.rCampo(tcStr, 'Origem');
        FLote[i].QtdeCupoms      := Leitor.rCampo(tcStr, 'QtdeCupoms');
        FLote[i].SituacaoLote    := Leitor.rCampo(tcStr, 'SituacaoLote');

        with FLote[i].Signature do
        begin
          URI             := Leitor.rAtributo('Reference URI=');
          DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
          SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
          X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
        end;

        j:= 0;
        while Leitor.rExtrai(2, 'Cfe', '', j + 1) <> '' do
        begin
          FLote.Items[i].InfCFe.New;
          FLote.Items[i].InfCFe[j].Chave    :=  Leitor.rCampo(tcStr, 'Chave');
          FLote.Items[i].InfCFe[j].nCupom   :=  Leitor.rCampo(tcStr, 'nCupom');
          FLote.Items[i].InfCFe[j].Situacao :=  Leitor.rCampo(tcStr, 'Situacao');
          FLote.Items[i].InfCFe[j].Erros    :=  Leitor.rCampo(tcStr, 'cfeErros');
          Inc(J);
        end;
        Inc(I);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

