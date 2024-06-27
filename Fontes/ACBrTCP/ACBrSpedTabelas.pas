{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
{******************************************************************************
|* Historico
|* Doação por "datilas" no link 
|* http://www.projetoacbr.com.br/forum/index.php?/topic/13345-novo-componete-acbrtabelassped/#entry80872
|* 04/04/2014 - Ajustes para o padrão ACBr
|*              por Isaque Pinheiro/Juliomar Marchetti               
******************************************************************************}

{$I ACBr.inc}

unit ACBrSpedTabelas;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrSocket, ACBrDownload;

type
  TACBrCodSistema = (csSpedFiscal, csSpedPisCofins, csSpedContabil, csSpedECF);

  EACBrTabelasSpedxception = class(Exception);

  { TACBrSpedTabela }

  TACBrSpedTabela = class
  private
    fId: string;
    fPacote: string;
    fTipo: string;
    fDesc: string;
    fVersao: string;
    fDtCriacao: TdateTime;
    fDtVersao: TdateTime;
    fHash: string;

  public
    property Id: string read fId write fId;
    property Pacote: string read fPacote write fPacote;
    property Tipo: string read fTipo write fTipo;
    property Desc: string read fDesc write fDesc;
    property Versao: string read fVersao write fVersao;
    property DtCriacao: TdateTime read fDtCriacao write fDtCriacao;
    property DtVersao: TdateTime read fDtVersao write fDtVersao;
    property Hash: string read fHash write fHash;
  end;

  { TACBrSpedTabelasClass }

  TACBrSpedTabelasClass = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrSpedTabela>{$EndIf})
  protected
    procedure SetObject(Index: integer; Item: TACBrSpedTabela);
    function GetObject(Index: integer): TACBrSpedTabela;
  public
    function Add(Obj: TACBrSpedTabela): integer;
    function New: TACBrSpedTabela;
    property Objects[Index: integer]: TACBrSpedTabela read GetObject write SetObject; default;
  end;

  { TACBrSpedTabelas}
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrSpedTabelas = class(TACBrHTTP)
  private
    fCodSistema: TACBrCodSistema;
    fTabelas: TACBrSpedTabelasClass;
    fUrlConsulta: string;
    fUrlDownload: string;
    fListou: boolean;
    fDirDestino: string;

    procedure SetCodSistema(const AValue: TACBrCodSistema);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ListarTabelas;
    function Download(const aId, aVersao, aName: string): boolean;
    property Tabelas: TACBrSpedTabelasClass read fTabelas write fTabelas;
  published
    property CodSistema: TACBrCodSistema read fCodSistema write SetCodSistema default csSpedFiscal;
    property UrlConsulta: string read fUrlConsulta write fUrlConsulta;
    property DirDestino: string read fDirDestino write fDirDestino;
  end;

implementation
 uses
   ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.XMLHTML;

constructor TACBrSpedTabelas.Create(AOwner: TComponent);
begin
  inherited;
  fTabelas := TACBrSpedTabelasClass.Create;
  fTabelas.Clear;
  fCodSistema := csSpedFiscal;
  fListou := False;
  fUrlConsulta := 'http://www.sped.fazenda.gov.br/spedtabelas/WsConsulta/WsConsulta.asmx/consultarVersoesTabelasExternas?codigoSistema=';
  fDirDestino := '.\';
end;

procedure TACBrSpedTabelasClass.SetObject(Index: integer; Item: TACBrSpedTabela);
begin
  inherited Items[Index] := Item;
end;

function TACBrSpedTabelasClass.GetObject(Index: integer): TACBrSpedTabela;
begin
  Result := TACBrSpedTabela(inherited Items[Index]);
end;

function TACBrSpedTabelasClass.New: TACBrSpedTabela;
begin
  Result := TACBrSpedTabela.Create;
  Add(Result);
end;

function TACBrSpedTabelasClass.Add(Obj: TACBrSpedTabela): integer;
begin
  Result := inherited Add(Obj);
end;

destructor TACBrSpedTabelas.Destroy;
begin
  fTabelas.Free;
  inherited Destroy;
end;

procedure TACBrSpedTabelas.SetCodSistema(const AValue: TACBrCodSistema);
begin
  if FCodSistema = AValue then
    exit;
  FCodSistema := AValue;
end;

procedure TACBrSpedTabelas.ListarTabelas;
var
  CodSis, Buffer, pct, dt: string;
  SL1: TStringList;
  i: integer;

  function CopyDeAte(Texto, TextIni, TextFim: string): string;
  var
    ContIni, ContFim: integer;
  begin
    Result := '';
    if (Pos(TextFim, Texto) <> 0) and (Pos(TextIni, Texto) <> 0) then
    begin
      ContIni := Pos(TextIni, Texto) + Length(TextIni);
      ContFim := Pos(TextFim, Texto);
      Result := Copy(Texto, ContIni, ContFim - ContIni);
    end;
  end;

begin
  fListou := False;
  case fCodSistema of
    csSpedFiscal :
      CodSis := 'spedfiscal';
    csSpedPisCofins :
      CodSis := 'spedPiscofins';
    csSpedContabil :
      CodSis := 'spedcontabil';
    csSpedECF :
      CodSis := 'spedecf';
  end;

  try
    Self.HTTPGet(fUrlConsulta + CodSis);
  except
    on E: Exception do
    begin
      raise EACBrTabelasSpedxception.Create('Erro ao consultar tabelas' + #13#10 + E.Message);
    end;
  end;

  Buffer := DecodeToString(HTTPResponse, RespIsUTF8);
  fUrlDownload := SeparaDados(Buffer, 'urlDownloadArquivo');
  Buffer := SeparaDados(Buffer, 'metadadosXml');
  Buffer := StringReplace(Buffer, '&lt;', '<', [rfReplaceAll]);
  Buffer := StringReplace(Buffer, '&gt;', '>' + sLineBreak, [rfReplaceAll]);

  sl1 := TStringList.Create;
  try
    sl1.Text := SeparaDados(Buffer, 'pacotes');
    for i := 0 to sl1.Count - 1 do
    begin
      if Pos('pacote cod', sl1[i]) > 0 then
      begin
        pct := CopyDeAte(sl1[i], 'desc="', '">');
      end;
      if Pos('</tabelas>', sl1[i]) > 0 then
      begin
        pct := '';
      end;
      if Pos('tabela id', sl1[i]) > 0 then
      begin
        fListou := True;
        with Tabelas.New do
        begin
          Id := CopyDeAte(sl1[i], 'id="', '" tipo');
          Tipo := CopyDeAte(sl1[i], 'tipo="', '" desc');
          Pacote := pct;
          Desc := CopyDeAte(sl1[i], 'desc="', '" versao');
          Versao := CopyDeAte(sl1[i], 'versao="', '" dtCriacao');
          dt := OnlyNumber(CopyDeAte(sl1[i], 'dtCriacao="', '" dtVersao'));
          DtCriacao := StoD(dt);
          dt := OnlyNumber(CopyDeAte(sl1[i], 'dtVersao="', '" hash'));
          DtVersao := StoD(dt);
          Hash := CopyDeAte(sl1[i], 'hash="', '" />');
        end;
      end;
    end;
  finally
    sl1.Free;
  end;
end;

function TACBrSpedTabelas.Download(const aId, aVersao, aName: string): boolean;
var
  Dow: TACBrDownload;
begin
  if not fListou then
    raise EACBrTabelasSpedxception.Create('Falta listar as tabelas');

  if Trim(aId) = '' then
    raise EACBrTabelasSpedxception.Create('Informe o Id da tabela');

  if Trim(aVersao) = '' then
    raise EACBrTabelasSpedxception.Create(ACBrStr('Informe a versão da tabela'));

  if Trim(fDirDestino) = '' then
    raise EACBrTabelasSpedxception.Create(ACBrStr('Informe o diretório onde o arquivo deve ser salvo'));

  if (not DirectoryExists(fDirDestino)) and (not ForceDirectories(fDirDestino)) then
    raise EACBrTabelasSpedxception.CreateFmt(ACBrStr('Não foi possível criar a pasta de destino: %s'),[fDirDestino]);

  Dow := TACBrDownload.Create(nil);
  try
    Dow.Protocolo := protHTTP;
    Dow.DownloadUrl := fUrlDownload + '?idTabela=' + aId + '&versao=' + aVersao;
    Dow.DownloadNomeArq := PathWithDelim(fDirDestino) + aName;

    if Length(aName) = 0 then
       Dow.DownloadNomeArq := PathWithDelim(fDirDestino) + aId + aVersao + '.txt';

    try
      Dow.StartDownload;
      Result := True;
    except
      Result := False;
    end;
  finally
    Dow.Free;
  end;
end;

end.
