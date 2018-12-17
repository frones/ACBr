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

unit pcteRetConsSitCTe;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor, pcteProcCTe,
  {pcteRetCancCTe,} pcteRetEnvEventoCTe;

type

  TRetEventoCTeCollection = class;
  TRetEventoCTeCollectionItem = class;
  TRetConsSitCTe = class;

  TRetCancCTe = class(TPersistent)
  private
    Fversao: String;
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchCTe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
  published
    property versao: String          read Fversao   write Fversao;
    property Id: String              read FId       write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property cUF: Integer            read FcUF      write FcUF;
    property chCTe: String           read FchCTe    write FchCTe;
    property dhRecbto: TDateTime     read FdhRecbto write FdhRecbto;
    property nProt: String           read FnProt    write FnProt;
  end;

  TRetEventoCTeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetEventoCTeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetEventoCTeCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRetEventoCTeCollectionItem;
    property Items[Index: Integer]: TRetEventoCTeCollectionItem read GetItem write SetItem; default;
  end;

  TRetEventoCTeCollectionItem = class(TCollectionItem)
  private
    FRetEventoCTe: TRetEventoCTe;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property RetEventoCTe: TRetEventoCTe read FRetEventoCTe write FRetEventoCTe;
  end;

  TRetConsSitCTe = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchCTe: String;
    FprotCTe: TProcCTe;
    FretCancCTe: TRetCancCTe;
    FprocEventoCTe: TRetEventoCTeCollection;
    FXMLprotCTe: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor                        read FLeitor        write FLeitor;
    property versao: String                         read Fversao        write Fversao;
    property tpAmb: TpcnTipoAmbiente                read FtpAmb         write FtpAmb;
    property verAplic: String                       read FverAplic      write FverAplic;
    property cStat: Integer                         read FcStat         write FcStat;
    property xMotivo: String                        read FxMotivo       write FxMotivo;
    property cUF: Integer                           read FcUF           write FcUF;
    property chCTe: String                          read FchCTe         write FchCTe;
    property protCTe: TProcCTe                      read FprotCTe       write FprotCTe;
    property retCancCTe: TRetCancCTe                read FretCancCTe    write FretCancCTe;
    property procEventoCTe: TRetEventoCTeCollection read FprocEventoCTe write FprocEventoCTe;
    property XMLprotCTe: String                     read FXMLprotCTe    write FXMLprotCTe;
  end;

implementation

{ TRetConsSitCTe }

constructor TRetConsSitCTe.Create;
begin
  FLeitor     := TLeitor.Create;
  FprotCTe    := TProcCTe.create;
  FretCancCTe := TRetCancCTe.create;
end;

destructor TRetConsSitCTe.Destroy;
begin
  FLeitor.Free;
  FprotCTe.Free;
  FretCancCTe.Free;

  if Assigned(procEventoCTe) then
    procEventoCTe.Free;

  inherited;
end;

function TRetConsSitCTe.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;

  FcStat           := 0;
  protCTe.cStat    := 0;
  retCancCTe.cStat := 0;

  try
    if leitor.rExtrai(1, 'retConsSitCTe') <> '' then
    begin
      Fversao   := Leitor.rAtributo('versao', 'retConsSitCTe');
      FtpAmb    := StrToTpAmb(ok, leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := leitor.rCampo(tcStr, 'verAplic');
      FcStat    := leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := leitor.rCampo(tcStr, 'xMotivo');
      FcUF      := leitor.rCampo(tcInt, 'cUF');

      // status 100 = Autorizado, 101 = Cancelado, 110 = Denegado, 301 = Denegado
      // A SEFAZ-MS esta retornando Status=129 como status de retorno da consulta
      // mas o status do CT-e consultado é 100
      if (FcStat in  [100, 101, 104, 110, 129]) or (FcStat = 301) then
      begin
        if (Leitor.rExtrai(1, 'protCTe') <> '') then
        begin
          // A propriedade XMLprotCTe contem o XML que traz o resultado do
          // processamento do CT-e.
          XMLprotCTe := Leitor.Grupo;

          if Leitor.rExtrai(2, 'infProt') <> '' then
          begin
            protCTe.Id       := Leitor.rAtributo('Id=', 'infProt');
            protCTe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
            protCTe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
            protCTe.chCTe    := Leitor.rCampo(tcStr, 'chCTe');
            protCTe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
            protCTe.nProt    := Leitor.rCampo(tcStr, 'nProt');
            protCTe.digVal   := Leitor.rCampo(tcStr, 'digVal');
            protCTe.cStat    := Leitor.rCampo(tcInt, 'cStat');
            protCTe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
            FchCTe           := protCTe.chCTe;
          end;
        end;
      end;

      if FcStat = 101 then
      begin
        if Leitor.rExtrai(1, 'infCanc') <> '' then
        begin
          retCancCTe.Id       := Leitor.rAtributo('Id=', 'infCanc');
          retCancCTe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
          retCancCTe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
          retCancCTe.cStat    := Leitor.rCampo(tcInt, 'cStat');
          retCancCTe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
          retCancCTe.cUF      := Leitor.rCampo(tcInt, 'cUF');
          retCancCTe.chCTe    := Leitor.rCampo(tcStr, 'chCTe');
          retCancCTe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          retCancCTe.nProt    := Leitor.rCampo(tcStr, 'nProt');
          FchCTe              := retCancCTe.chCTe;
        end;
      end;

      if Assigned(procEventoCTe) then
        procEventoCTe.Free;

      procEventoCTe := TRetEventoCTeCollection.Create(Self);
      i := 0;
      while Leitor.rExtrai(1, 'procEventoCTe', '', i + 1) <> '' do
      begin
        procEventoCTe.Add;
        procEventoCTe.Items[i].RetEventoCTe.Leitor.Arquivo := Leitor.Grupo;
        procEventoCTe.Items[i].RetEventoCTe.XML := Leitor.Grupo;
        procEventoCTe.Items[i].RetEventoCTe.LerXml;
        inc(i);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetEventoCTeCollection }

function TRetEventoCTeCollection.Add: TRetEventoCTeCollectionItem;
begin
  Result := TRetEventoCTeCollectionItem(inherited Add);
  Result.create;
end;

constructor TRetEventoCTeCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TRetEventoCTeCollectionItem);
end;

function TRetEventoCTeCollection.GetItem(Index: Integer): TRetEventoCTeCollectionItem;
begin
  Result := TRetEventoCTeCollectionItem(inherited GetItem(Index));
end;

procedure TRetEventoCTeCollection.SetItem(Index: Integer; Value: TRetEventoCTeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRetEventoCTeCollectionItem }

constructor TRetEventoCTeCollectionItem.Create;
begin
  FRetEventoCTe := TRetEventoCTe.Create;
end;

destructor TRetEventoCTeCollectionItem.Destroy;
begin
  FRetEventoCTe.Free;

  inherited;
end;

end.

