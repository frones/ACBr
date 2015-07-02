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

unit pcnRetConsNFeDest;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnConversaoNFe, pcnLeitor;

type
  TresNFe            = class;
  TresCanc           = class;
  TresCCe            = class;
  TRetCollection     = class;
  TRetCollectionItem = class;
  TRetConsNFeDest    = class;

  TresNFe = class
  private
    FdhRecbto: TDateTime;
    FdEmi: TDateTime;
    FtpNF: TpcnTipoNFe;
    FdigVal: String;
    FchNFe: String;
    FvNF: Double;
    FxNome: String;
    FCNPJCPF: String;
    FIE: String;
    FcSitConf: TpcnSituacaoManifDest;
    FNSU: String;
    FcSitNFe: TSituacaoDFe;
  public
    property NSU: String                     read FNSU      write FNSU;
    property chNFe: String                   read FchNFe    write FchNFe;
    property CNPJCPF: String                 read FCNPJCPF  write FCNPJCPF;
    property xNome: String                   read FxNome    write FxNome;
    property IE: String                      read FIE       write FIE;
    property dEmi: TDateTime                 read FdEmi     write FdEmi;
    property tpNF: TpcnTipoNFe               read FtpNF     write FtpNF;
    property vNF: Double                     read FvNF      write FvNF;
    property digVal: String                  read FdigVal   write FdigVal;
    property dhRecbto: TDateTime             read FdhRecbto write FdhRecbto;
    property cSitNFe: TSituacaoDFe           read FcSitNFe  write FcSitNFe;
    property cSitConf: TpcnSituacaoManifDest read FcSitConf write FcSitConf;
  end;

  TresCanc = class
  private
    FdhRecbto: TDateTime;
    FdEmi: TDateTime;
    FtpNF: TpcnTipoNFe;
    FdigVal: String;
    FchNFe: String;
    FvNF: Double;
    FxNome: String;
    FCNPJCPF: String;
    FIE: String;
    FcSitConf: TpcnSituacaoManifDest;
    FNSU: String;
    FcSitNFe: TSituacaoDFe;
  public
    property NSU: String                     read FNSU      write FNSU;
    property chNFe: String                   read FchNFe    write FchNFe;
    property CNPJCPF: String                 read FCNPJCPF  write FCNPJCPF;
    property xNome: String                   read FxNome    write FxNome;
    property IE: String                      read FIE       write FIE;
    property dEmi: TDateTime                 read FdEmi     write FdEmi;
    property tpNF: TpcnTipoNFe               read FtpNF     write FtpNF;
    property vNF: Double                     read FvNF      write FvNF;
    property digVal: String                  read FdigVal   write FdigVal;
    property dhRecbto: TDateTime             read FdhRecbto write FdhRecbto;
    property cSitNFe: TSituacaoDFe           read FcSitNFe  write FcSitNFe;
    property cSitConf: TpcnSituacaoManifDest read FcSitConf write FcSitConf;
  end;

  TresCCe = class
  private
    FdhRecbto: TDateTime;
    FtpNF: TpcnTipoNFe;
    FchNFe: String;
    FdhEvento: TDateTime;
    FxCorrecao: String;
    FtpEvento: TpcnTpEvento;
    FdescEvento: String;
    FNSU: String;
    FnSeqEvento: ShortInt;
  public
    property NSU: String            read FNSU        write FNSU;
    property chNFe: String          read FchNFe      write FchNFe;
    property dhEvento: TDateTime    read FdhEvento   write FdhEvento;
    property tpEvento: TpcnTpEvento read FtpEvento   write FtpEvento;
    property nSeqEvento: ShortInt   read FnSeqEvento write FnSeqEvento;
    property descEvento: String     read FdescEvento write FdescEvento;
    property xCorrecao: String      read FxCorrecao  write FxCorrecao;
    property tpNF: TpcnTipoNFe      read FtpNF       write FtpNF;
    property dhRecbto: TDateTime    read FdhRecbto   write FdhRecbto;
  end;

  TRetCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRetCollectionItem;
    property Items[Index: Integer]: TRetCollectionItem read GetItem write SetItem; default;
  end;

  TRetCollectionItem = class(TCollectionItem)
  private
    FresNFe: TresNFe;
    FresCanc: TresCanc;
    FresCCe: TresCCe;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property resNFe: TresNFe   read FresNFe  write FresNFe;
    property resCanc: TresCanc read FresCanc write FresCanc;
    property resCCe: TresCCe   read FresCCe  write FresCCe;
  end;

  TRetConsNFeDest = class(TPersistent)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FLeitor: TLeitor;
    FcStat: Integer;
    FdhResp: TDateTime;
    FxMotivo: String;
    FindCont: TpcnIndicadorContinuacao;
    FultNSU: String;
    Fret: TRetCollection;
    FXML: AnsiString;

    procedure Setret(const Value: TRetCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property versao: String                    read Fversao   write Fversao;
    property Leitor: TLeitor                   read FLeitor   write FLeitor;
    property tpAmb: TpcnTipoAmbiente           read FtpAmb    write FtpAmb;
    property verAplic: String                  read FverAplic write FverAplic;
    property cStat: Integer                    read FcStat    write FcStat;
    property xMotivo: String                   read FxMotivo  write FxMotivo;
    property dhResp: TDateTime                 read FdhResp   write FdhResp;
    property indCont: TpcnIndicadorContinuacao read FindCont  write FindCont;
    property ultNSU: String                    read FultNSU   write FultNSU;
    property ret: TRetCollection               read Fret      write Setret;
    property XML: AnsiString                   read FXML      write FXML;
  end;

implementation

{ TRetCollection }

function TRetCollection.Add: TRetCollectionItem;
begin
  Result := TRetCollectionItem(inherited Add);
  Result.create;
end;

constructor TRetCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TRetCollectionItem);
end;

function TRetCollection.GetItem(
  Index: Integer): TRetCollectionItem;
begin
  Result := TRetCollectionItem(inherited GetItem(Index));
end;

procedure TRetCollection.SetItem(Index: Integer;
  Value: TRetCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRetCollectionItem }

constructor TRetCollectionItem.Create;
begin
  FresNFe  := TresNFe.Create;
  FresCanc := TresCanc.Create;
  FresCCe  := TresCCe.Create;
end;

destructor TRetCollectionItem.Destroy;
begin
  FresNFe.Free;
  FresCanc.Free;
  FresCCe.Free;
  inherited;
end;

{ TRetConsNFeDest }

procedure TRetConsNFeDest.Setret(const Value: TRetCollection);
begin
  Fret.Assign(Value);
end;

constructor TRetConsNFeDest.Create;
begin
  FLeitor := TLeitor.Create;
  Fret    := TRetCollection.Create(Self);
end;

destructor TRetConsNFeDest.Destroy;
begin
  FLeitor.Free;
  Fret.Free;
  inherited;
end;

function TRetConsNFeDest.LerXml: Boolean;
var
  ok: Boolean;
  i : Integer;
begin
  Result := False;
  try
    FXML := Self.Leitor.Arquivo;

    if (Leitor.rExtrai(1, 'retConsNFeDest') <> '') then
    begin
      (*IR02 *)Fversao   := Leitor.rAtributo('versao');
      (*IR03 *)FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      (*IR04 *)FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      (*IR05 *)FcStat    := Leitor.rCampo(tcInt, 'cStat');
      (*IR06 *)FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      (*IR07 *)FdhResp   := Leitor.rCampo(tcDatHor, 'dhResp');
      (*IR08 *)FindCont  := StrToIndicadorContinuacao(ok, Leitor.rCampo(tcStr, 'indCont'));
      (*IR09 *)FultNSU   := Leitor.rCampo(tcStr, 'ultNSU');

      i := 0;
      while Leitor.rExtrai(2, 'ret', '', i + 1) <> '' do
      begin
        Fret.Add;

        if (Leitor.rExtrai(3, 'resNFe') <> '') then
        begin
          (*IR12 *)Fret.Items[i].FresNFe.FNSU      := Leitor.rAtributo('NSU');
          (*IR13 *)Fret.Items[i].FresNFe.chNFe     := Leitor.rCampo(tcStr, 'chNFe');
          (*IR14 *)Fret.Items[i].FresNFe.FCNPJCPF  := Leitor.rCampo(tcStr, 'CNPJ');

          if Fret.Items[i].FresNFe.FCNPJCPF = '' then
            (*IR15 *)Fret.Items[i].FresNFe.FCNPJCPF := Leitor.rCampo(tcStr, 'CPF');

          (*IR16 *)Fret.Items[i].FresNFe.FxNome    := Leitor.rCampo(tcStr, 'xNome');
          (*IR17 *)Fret.Items[i].FresNFe.FIE       := Leitor.rCampo(tcStr, 'IE');
          (*IR18 *)Fret.Items[i].FresNFe.FdEmi     := Leitor.rCampo(tcDat, 'dEmi');
          (*IR19 *)Fret.Items[i].FresNFe.FtpNF     := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));
          (*IR20 *)Fret.Items[i].FresNFe.FvNF      := Leitor.rCampo(tcDe2, 'vNF');
          (*IR21 *)Fret.Items[i].FresNFe.FdigVal   := Leitor.rCampo(tcStr, 'digVal');
          (*IR22 *)Fret.Items[i].FresNFe.FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          (*IR23 *)Fret.Items[i].FresNFe.FcSitNFe  := StrToSituacaoDFe(ok, Leitor.rCampo(tcStr, 'cSitNFe'));
          (*IR24 *)Fret.Items[i].FresNFe.FcSitConf := StrToSituacaoManifDest(ok, Leitor.rCampo(tcStr, 'cSitConf'));
        end;

        if (Leitor.rExtrai(3, 'resCanc') <> '') then
        begin
          (*IR26 *)Fret.Items[i].FresCanc.FNSU      := Leitor.rAtributo('NSU');
          (*IR27 *)Fret.Items[i].FresCanc.chNFe     := Leitor.rCampo(tcStr, 'chNFe');
          (*IR28 *)Fret.Items[i].FresCanc.FCNPJCPF  := Leitor.rCampo(tcStr, 'CNPJ');

          if Fret.Items[i].FresCanc.FCNPJCPF = '' then
            (*IR29 *)Fret.Items[i].FresCanc.FCNPJCPF := Leitor.rCampo(tcStr, 'CPF');

          (*IR30 *)Fret.Items[i].FresCanc.FxNome    := Leitor.rCampo(tcStr, 'xNome');
          (*IR31 *)Fret.Items[i].FresCanc.FIE       := Leitor.rCampo(tcStr, 'IE');
          (*IR32 *)Fret.Items[i].FresCanc.FdEmi     := Leitor.rCampo(tcDat, 'dEmi');
          (*IR33 *)Fret.Items[i].FresCanc.FtpNF     := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));
          (*IR34 *)Fret.Items[i].FresCanc.FvNF      := Leitor.rCampo(tcDe2, 'vNF');
          (*IR35 *)Fret.Items[i].FresCanc.FdigVal   := Leitor.rCampo(tcStr, 'digVal');
          (*IR36 *)Fret.Items[i].FresCanc.FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
          (*IR37 *)Fret.Items[i].FresCanc.FcSitNFe  := StrToSituacaoDFe(ok, Leitor.rCampo(tcStr, 'cSitNFe'));
          (*IR38 *)Fret.Items[i].FresCanc.FcSitConf := StrToSituacaoManifDest(ok, Leitor.rCampo(tcStr, 'cSitConf'));
        end;

        if (Leitor.rExtrai(3, 'resCCe') <> '') then
        begin
          (*IR40 *)Fret.Items[i].FresCCe.FNSU        := Leitor.rAtributo('NSU');
          (*IR41 *)Fret.Items[i].FresCCe.chNFe       := Leitor.rCampo(tcStr, 'chNFe');
          (*IR42 *)Fret.Items[i].FresCCe.FdhEvento   := Leitor.rCampo(tcDatHor, 'dhEvento');
          (*IR43 *)Fret.Items[i].FresCCe.FtpEvento   := StrToTpEvento(ok, Leitor.rCampo(tcStr, 'tpEvento'));
          (*IR44 *)Fret.Items[i].FresCCe.FnSeqEvento := Leitor.rCampo(tcInt, 'nSeqEvento');
          (*IR45 *)Fret.Items[i].FresCCe.FdescEvento := Leitor.rCampo(tcStr, 'descEvento');
          (*IR46 *)Fret.Items[i].FresCCe.FxCorrecao  := Leitor.rCampo(tcStr, 'xCorrecao');
          (*IR47 *)Fret.Items[i].FresCCe.FtpNF       := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));
          (*IR48 *)Fret.Items[i].FresCCe.FdhRecbto   := Leitor.rCampo(tcDatHor, 'dhRecbto');
        end;

        inc(i);
      end;

      if i = 0
       then Fret.Add;

      Result := True;
    end;
  except
    result := False;
  end;
end;

end.
