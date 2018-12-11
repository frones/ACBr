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

unit pcnRetDownloadNFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, synacode;

type
  TRetNFeCollection     = class;
  TRetNFeCollectionItem = class;
  TRetDownloadNFe       = class;

  TRetNFeCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TRetNFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetNFeCollectionItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TRetNFeCollectionItem;
    property Items[Index: Integer]: TRetNFeCollectionItem read GetItem write SetItem; default;
  end;

  TRetNFeCollectionItem = class(TCollectionItem)
  private
    FchNFe: String;
    FcStat: Integer;
    FxMotivo: String;
    FprocNFe: AnsiString;
    FdhEmi: TDateTime;

    FNFeZip: String;
    FProtNFeZip: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property chNFe: String       read FchNFe   write FchNFe;
    property cStat: Integer      read FcStat   write FcStat;
    property xMotivo: String     read FxMotivo write FxMotivo;
    property procNFe: AnsiString read FprocNFe write FprocNFe;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;

    property NFeZip: String      read FNFeZip     write FNFeZip;
    property ProtNFeZip: String  read FProtNFeZip write FProtNFeZip;
  end;

  TRetDownloadNFe = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fversao : String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FretNFe: TRetNFeCollection;
    FXML: AnsiString;

    procedure SetretNFe(const Value: TRetNFeCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    function LerXMLFromFile(CaminhoArquivo: String): Boolean;
  published
    property Leitor: TLeitor           read FLeitor   write FLeitor;
    property versao: String            read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente   read FtpAmb    write FtpAmb;
    property verAplic: String          read FverAplic write FverAplic;
    property cStat: Integer            read FcStat    write FcStat;
    property xMotivo: String           read FxMotivo  write FxMotivo;
    property dhResp: TDateTime         read FdhResp   write FdhResp;
    property retNFe: TRetNFeCollection read FretNFe   write SetretNFe;
    property XML: AnsiString           read FXML      write FXML;
  end;

implementation

uses
  pcnGerador, ACBrUtil;

{ TRetNFeCollection }

function TRetNFeCollection. Add: TRetNFeCollectionItem;
begin
  Result := TRetNFeCollectionItem(inherited Add);
  Result.create;
end;

constructor TRetNFeCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TRetNFeCollectionItem);
end;

function TRetNFeCollection.GetItem(
  Index: Integer): TRetNFeCollectionItem;
begin
  Result := TRetNFeCollectionItem(inherited GetItem(Index));
end;

procedure TRetNFeCollection.SetItem(Index: Integer;
  Value: TRetNFeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TRetNFeCollectionItem }

constructor TRetNFeCollectionItem.Create;
begin

end;

destructor TRetNFeCollectionItem.Destroy;
begin

  inherited;
end;

{ TRetDownloadNFe }

procedure TRetDownloadNFe.SetretNFe(const Value: TRetNFeCollection);
begin
  FretNFe.Assign(Value);
end;

constructor TRetDownloadNFe.Create;
begin
  FLeitor := TLeitor.Create;
  FretNFe := TRetNFeCollection.Create(Self);
end;

destructor TRetDownloadNFe.Destroy;
begin
  FLeitor.Free;
  FretNFe.Free;
  inherited;
end;

function TRetDownloadNFe.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
  StrAux, versao: String;
  StrDecod: AnsiString;

  function LerDataHora(ADataHora: String): TDateTime;
  begin
    result := EncodeDate(StrToInt(copy(ADataHora, 01, 4)), StrToInt(copy(ADataHora, 06, 2)), StrToInt(copy(ADataHora, 09, 2))) +
              EncodeTime(StrToInt(copy(ADataHora, 12, 2)), StrToInt(copy(ADataHora, 15, 2)), StrToInt(copy(ADataHora, 18, 2)), 0);
  end;
begin
  Result := False;
  try
    FXML := Self.Leitor.Arquivo;

    if (Leitor.rExtrai(1, 'retDownloadNFe') <> '') then
    begin
      (*JR02 *)Fversao   := Leitor.rAtributo('versao');
      (*JR03 *)FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      (*JR04 *)FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      (*JR05 *)FcStat    := Leitor.rCampo(tcInt, 'cStat');
      (*JR06 *)FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      (*JR07 *)FdhResp   := Leitor.rCampo(tcDatHor, 'dhResp');
      i := 0;
      while Leitor.rExtrai(2, 'retNFe', '', i + 1) <> '' do
      begin
        FretNFe.Add;
        (*JR09 *)FretNFe.Items[i].FchNFe   := Leitor.rCampo(tcStr, 'chNFe');
        (*JR10 *)FretNFe.Items[i].FcStat   := Leitor.rCampo(tcInt, 'cStat');
        (*JR11 *)FretNFe.Items[i].FxMotivo := Leitor.rCampo(tcStr, 'xMotivo');

        if pos('procNFeZip', Leitor.Grupo) > 0 then
        begin
          // XML da NF-e
          StrAux := RetornarConteudoEntre(Leitor.Grupo, '<procNFeZip>', '</procNFeZip');
          StrDecod := DecodeBase64(StrAux);
          FretNFe.Items[i].FNFeZip := InserirDeclaracaoXMLSeNecessario(UnZip(StrDecod));
          FretNFe.Items[i].FdhEmi := LerDataHora(RetornarConteudoEntre(FretNFe.Items[i].FNFeZip, '<dhEmi>', '</dhEmi'));
          FretNFe.Items[i].FprocNFe := StringReplace(FretNFe.Items[i].FNFeZip,
                                         '<' + ENCODING_UTF8 + '>', '',
                                         [rfReplaceAll]);
        end;

        if pos('procNFe', Leitor.Grupo) > 0 then
        begin
          FretNFe.Items[i].FdhEmi := LerDataHora(RetornarConteudoEntre(Leitor.Grupo, '<dhEmi>', '</dhEmi'));
          FretNFe.Items[i].FprocNFe := StringReplace(SeparaDados(Leitor.Grupo, 'procNFe'),
                                         '<' + ENCODING_UTF8 + '>', '',
                                         [rfReplaceAll]);
        end;

        if (Leitor.rExtrai(3, 'procNFeGrupoZip') <> '') then
        begin
          // XML da NF-e
          StrAux := RetornarConteudoEntre(Leitor.Grupo, '<NFeZip>', '</NFeZip');
          StrDecod := DecodeBase64(StrAux);
          FretNFe.Items[i].FNFeZip := InserirDeclaracaoXMLSeNecessario(UnZip(StrDecod));
          FretNFe.Items[i].FdhEmi := LerDataHora(RetornarConteudoEntre(FretNFe.Items[i].FNFeZip, '<dhEmi>', '</dhEmi'));

          // XML do Protocolo da NF-e
          StrAux := RetornarConteudoEntre(Leitor.Grupo, '<protNFeZip>', '</protNFeZip');
          StrDecod := DecodeBase64(StrAux);
          FretNFe.Items[i].FProtNFeZip := InserirDeclaracaoXMLSeNecessario(UnZip(StrDecod));

          versao := RetornarConteudoEntre(FretNFe.Items[i].FNFeZip, 'versao="', '">');

          FretNFe.Items[i].FprocNFe :=
                  '<nfeProc versao="' + copy(versao, 1, 4) + '" ' + NAME_SPACE + '>' +
                    FretNFe.Items[i].FNFeZip +
                    '<protNFe' +
                      RetornarConteudoEntre(FretNFe.Items[i].FProtNFeZip,
                                                     '<protNFe', '</protNFe>') +
                    '</protNFe>' +
                  '</nfeProc>';
        end;

        inc(i);
      end;
      Result := True;
    end;
  except
    result := False;
  end;
end;

function TRetDownloadNFe.LerXMLFromFile(CaminhoArquivo: String): Boolean;
var
  ArqDown: TStringList;
begin
  ArqDown := TStringList.Create;
  try
    ArqDown.LoadFromFile(CaminhoArquivo);
    Self.Leitor.Arquivo := ArqDown.Text;
    Result := LerXml;
  finally
    ArqDown.Free;
  end;
end;

end.
