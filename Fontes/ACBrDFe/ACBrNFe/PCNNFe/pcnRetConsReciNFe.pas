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

unit pcnRetConsReciNFe;

interface

uses
  SysUtils, Classes, pcnConversao, pcnLeitor;

type

  TRetConsReciNFe        = class;
  TProtNFeCollection     = class;
  TProtNFeCollectionItem = class;

  TRetConsReciNFe = class
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FnRec: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FcMsg: Integer;
    FxMsg: String;
    FProtNFe: TProtNFeCollection;

    procedure SetProtNFe(const Value: TProtNFeCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXML: Boolean;
  published
    property Leitor: TLeitor             read FLeitor   write FLeitor;
    property versao: String              read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente     read FtpAmb    write FtpAmb;
    property verAplic: String            read FverAplic write FverAplic;
    property nRec: String                read FnRec     write FnRec;
    property cStat: Integer              read FcStat    write FcStat;
    property xMotivo: String             read FxMotivo  write FxMotivo;
    property cUF: Integer                read FcUF      write FcUF;
    property cMsg: Integer               read FcMsg     write FcMsg;
    property xMsg: String                read FxMsg     write FxMsg;
    property ProtNFe: TProtNFeCollection read FProtNFe  write SetProtNFe;
  end;

  TProtNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TProtNFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TProtNFeCollectionItem);
  public
    constructor Create(AOwner: TRetConsReciNFe); reintroduce;
    function Add: TProtNFeCollectionItem;
    property Items[Index: Integer]: TProtNFeCollectionItem read GetItem write SetItem; default;
  end;

  TProtNFeCollectionItem = class(TCollectionItem)
  private
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchNFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FXMLprotNFe: String;
  published
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: String        read FverAplic   write FverAplic;
    property chNFe: String           read FchNFe      write FchNFe;
    property dhRecbto: TDateTime     read FdhRecbto   write FdhRecbto;
    property nProt: String           read FnProt      write FnProt;
    property digVal: String          read FdigVal     write FdigVal;
    property cStat: Integer          read FcStat      write FcStat;
    property xMotivo: String         read FxMotivo    write FxMotivo;
    property XMLprotNFe: String      read FXMLprotNFe write FXMLprotNFe;
  end;

implementation

{ TRetConsReciNFe }

constructor TRetConsReciNFe.Create;
begin
  inherited Create;
  FLeitor  := TLeitor.Create;
  FProtNFe := TProtNFeCollection.Create(self);
end;

destructor TRetConsReciNFe.Destroy;
begin
  FLeitor.Free;
  FProtNFe.Free;
  inherited;
end;

procedure TRetConsReciNFe.SetProtNFe(const Value: TProtNFeCollection);
begin
  FProtNFe.Assign(Value);
end;

{ TProtNFeCollection }

constructor TProtNFeCollection.Create(AOwner: TRetConsReciNFe);
begin
  inherited Create(TProtNFeCollectionItem);
end;

function TProtNFeCollection.Add: TProtNFeCollectionItem;
begin
  Result := TProtNFeCollectionItem(inherited Add);
end;

function TProtNFeCollection.GetItem(Index: Integer): TProtNFeCollectionItem;
begin
  Result := TProtNFeCollectionItem(inherited GetItem(Index));
end;

procedure TProtNFeCollection.SetItem(Index: Integer; Value: TProtNFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRetConsReciNFe.LerXML: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;

    if Leitor.rExtrai(1, 'retConsReciNFe') <> '' then
    begin
//      Leitor.Grupo := Leitor.Arquivo;

               Fversao   := Leitor.rAtributo('versao');
      (*BR03 *)FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      (*BR04 *)FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      (*BR04a*)FnRec     := Leitor.rCampo(tcStr, 'nRec');
      (*BR05 *)FcStat    := Leitor.rCampo(tcInt, 'cStat');
      (*BR06 *)FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      (*BR06a*)FcUF      := Leitor.rCampo(tcInt, 'cUF');
      (*BR06b*)FcMsg     := Leitor.rCampo(tcInt, 'cMsg');
      (*BR06c*)FxMsg     := Leitor.rCampo(tcStr, 'xMsg');

      i := 0;
      while (FcStat = 104) and (Leitor.rExtrai(1, 'protNFe', '', i + 1) <> '') do
      begin
        ProtNFe.Add;

        // A propriedade XMLprotNFe contem o XML que traz o resultado do
        // processamento da NF-e.
        ProtNFe[i].XMLprotNFe := Leitor.Grupo;

        if Leitor.rExtrai(2, 'infProt') <> '' then
        begin
          (*PR05*)ProtNFe[i].FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
          (*PR06*)ProtNFe[i].FverAplic := Leitor.rCampo(tcStr, 'verAplic');
          (*PR07*)ProtNFe[i].FchNFe    := Leitor.rCampo(tcStr, 'chNFe');
          (*PR08*)ProtNFe[i].FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          (*PR09*)ProtNFe[i].FnProt    := Leitor.rCampo(tcStr, 'nProt');
          (*PR10*)ProtNFe[i].FdigVal   := Leitor.rCampo(tcStr, 'digVal');
          (*PR11*)ProtNFe[i].FcStat    := Leitor.rCampo(tcInt, 'cStat');
          (*PR12*)ProtNFe[i].FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
        end;
        inc(i);
      end;

//      if i = 0 then ProtNFe.Add;

      Result := True;
    end;
  except
    result := False;
  end;
end;

end.

