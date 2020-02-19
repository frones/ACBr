{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{$I ACBr.inc}

unit pcnEnvEventoNF3e;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador, pcnEventoNF3e, pcnConsts, pcnNF3eConsts, pcnSignature;

type
  EventoException = class(Exception);

  TInfEventoCollectionItem = class(TObject)
  private
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FRetInfEvento: TRetInfEvento;
  public
    constructor Create;
    destructor Destroy; override;

    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property signature: Tsignature       read Fsignature    write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    function New: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  { TEventoNF3e }

  TEventoNF3e = class(TObject)
  private
    FGerador: TGerador;
    FidLote: Integer;
    FEvento: TInfEventoCollection;
    FVersao: String;

    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
    function LerFromIni(const AIniString: String; CCe: Boolean = True): Boolean;

    property Gerador: TGerador            read FGerador write FGerador;
    property idLote: Integer              read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: String               read FVersao  write FVersao;
  end;

implementation

uses
  IniFiles,
  pcnRetEnvEventoNF3e, pcnAuxiliar, pcnConversaoNF3e,
  ACBrUtil, ACBrDFeUtil;

{ TInfEventoCollection }

function TInfEventoCollection.GetItem(
  Index: Integer): TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TInfEventoCollection.SetItem(Index: Integer;
  Value: TInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfEventoCollection.New: TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
  inherited Create;

  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  fsignature.Free;
  FRetInfEvento.Free;

  inherited;
end;

{ TEventoNF3e }

constructor TEventoNF3e.Create;
begin
  inherited Create;

  FGerador := TGerador.Create;
  FEvento  := TInfEventoCollection.Create();
end;

destructor TEventoNF3e.Destroy;
begin
  FGerador.Free;
  FEvento.Free;

  inherited;
end;

function TEventoNF3e.ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
begin
  case tpEvento of
    teCancelamento: Result := IntToStr(Self.idLote) + '-can-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoNF3e.GerarXML: Boolean;
var
  i: Integer;
  sDoc, sModelo: String;
begin
  Gerador.ArquivoFormatoXML := '';
//  Gerador.wGrupo('envEvento ' + NAME_SPACE_NF3e + ' versao="' + Versao + '"');
//  Gerador.wCampo(tcInt, 'HP03', 'idLote', 001, 015, 1, FidLote, DSC_IDLOTE);

  for i := 0 to Evento.Count - 1 do
  begin
    sModelo := Copy(OnlyNumber(Evento.Items[i].InfEvento.chNF3e), 21, 2);

    Evento.Items[i].InfEvento.id := 'ID'+
                                      Evento.Items[i].InfEvento.TipoEvento +
                                      OnlyNumber(Evento.Items[i].InfEvento.chNF3e) +
                                      Format('%.2d', [Evento.Items[i].InfEvento.nSeqEvento]);

    Gerador.wGrupo('eventoNF3e ' + NAME_SPACE_NF3e + ' versao="' + Versao + '"');
    Gerador.wGrupo('infEvento Id="' + Evento.Items[i].InfEvento.id + '"');

    if Length(Evento.Items[i].InfEvento.id) < 54 then
      Gerador.wAlerta('FP04', 'ID', '', 'ID de Evento inválido');

    Gerador.wCampo(tcInt, 'FP05', 'cOrgao', 1, 2, 1, FEvento.Items[i].FInfEvento.cOrgao);
    Gerador.wCampo(tcStr, 'FP06', 'tpAmb ', 1, 1, 1, TpAmbToStr(Evento.Items[i].InfEvento.tpAmb), DSC_TPAMB);

    sDoc := OnlyNumber(Evento.Items[i].InfEvento.CNPJ);

    if EstaVazio(sDoc) then
      sDoc := ExtrairCNPJChaveAcesso(Evento.Items[i].InfEvento.chNF3e);

    case Length( sDoc ) of
      14: begin
            Gerador.wCampo(tcStr, 'HP10', 'CNPJ', 14, 14, 1, sDoc , DSC_CNPJ);
            if not ValidarCNPJ( sDoc ) then
              Gerador.wAlerta('FP07', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
          end;
      11: begin
            Gerador.wCampo(tcStr, 'HP11', 'CPF ', 11, 11, 1, sDoc, DSC_CPF);
            if not ValidarCPF( sDoc ) then
              Gerador.wAlerta('FP07', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
          end;
    end;

    Gerador.wCampo(tcStr, 'FP08', 'chNF3e', 44, 44, 1, Evento.Items[i].InfEvento.chNF3e, DSC_CHAVE);

    if not ValidarChave(Evento.Items[i].InfEvento.chNF3e) then
      Gerador.wAlerta('FP08', 'chNF3e', '', 'Chave de NF3e inválida');

    Gerador.wCampo(tcStr, 'FP09', 'dhEvento  ', 1, 50, 1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Evento.Items[i].InfEvento.dhEvento)+
                                                          GetUTC(CodigoParaUF(Evento.Items[i].InfEvento.cOrgao), Evento.Items[i].InfEvento.dhEvento));
    Gerador.wCampo(tcInt, 'FP10', 'tpEvento  ', 6, 06, 1, Evento.Items[i].InfEvento.TipoEvento);
    Gerador.wCampo(tcInt, 'FP11', 'nSeqEvento', 1, 02, 1, Evento.Items[i].InfEvento.nSeqEvento);

    Gerador.wGrupo('detEvento versaoEvento="' +  Versao + '"');
    Gerador.wCampo(tcStr, 'HP14', 'descEvento', 4, 60, 1, Evento.Items[i].InfEvento.DescEvento);

    case Evento.Items[i].InfEvento.tpEvento of
      teCancelamento:
        begin
          Gerador.wCampo(tcStr, 'FP15', 'nProt', 15, 015, 1, Evento.Items[i].InfEvento.detEvento.nProt);
          Gerador.wCampo(tcStr, 'FP16', 'xJust', 15, 255, 1, Evento.Items[i].InfEvento.detEvento.xJust);
        end;
    end;

    Gerador.wGrupo('/detEvento');
    Gerador.wGrupo('/infEvento');

    if Evento.Items[i].signature.URI <> '' then
    begin
      Evento.Items[i].signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      Evento.Items[i].signature.GerarXML;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + Evento.Items[i].signature.Gerador.ArquivoFormatoXML;
    end;

    Gerador.wGrupo('/eventoNF3e');
  end;

//  Gerador.wGrupo('/envEvento');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TEventoNF3e.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoNF3e.LerXML(const CaminhoArquivo: String): Boolean;
var
  ArqEvento    : TStringList;
begin
  ArqEvento := TStringList.Create;
  try
     ArqEvento.LoadFromFile(CaminhoArquivo);
     Result := LerXMLFromString(ArqEvento.Text);
  finally
     ArqEvento.Free;
  end;
end;

function TEventoNF3e.LerXMLFromString(const AXML: String): Boolean;
var
  RetEventoNF3e : TRetEventoNF3e;
begin
  RetEventoNF3e := TRetEventoNF3e.Create;
  try
    RetEventoNF3e.Leitor.Arquivo := AXML;
    Result := RetEventoNF3e.LerXml;
    with FEvento.New do
    begin
      infEvento.ID            := RetEventoNF3e.InfEvento.id;
      infEvento.cOrgao        := RetEventoNF3e.InfEvento.cOrgao;
      infEvento.tpAmb         := RetEventoNF3e.InfEvento.tpAmb;
      infEvento.CNPJ          := RetEventoNF3e.InfEvento.CNPJ;
      infEvento.chNF3e         := RetEventoNF3e.InfEvento.chNF3e;
      infEvento.dhEvento      := RetEventoNF3e.InfEvento.dhEvento;
      infEvento.tpEvento      := RetEventoNF3e.InfEvento.tpEvento;
      infEvento.nSeqEvento    := RetEventoNF3e.InfEvento.nSeqEvento;
      infEvento.VersaoEvento  := RetEventoNF3e.InfEvento.VersaoEvento;

      infEvento.DetEvento.descEvento := RetEventoNF3e.InfEvento.DetEvento.descEvento;
      infEvento.DetEvento.xCorrecao  := RetEventoNF3e.InfEvento.DetEvento.xCorrecao;
      infEvento.DetEvento.xCondUso   := RetEventoNF3e.InfEvento.DetEvento.xCondUso;
      infEvento.DetEvento.nProt      := RetEventoNF3e.InfEvento.DetEvento.nProt;
      infEvento.DetEvento.xJust      := RetEventoNF3e.InfEvento.DetEvento.xJust;
      infEvento.DetEvento.chNF3eRef   := RetEventoNF3e.InfEvento.DetEvento.chNF3eRef;

      infEvento.detEvento.cOrgaoAutor := RetEventoNF3e.InfEvento.detEvento.cOrgaoAutor;
      infEvento.detEvento.tpAutor     := RetEventoNF3e.InfEvento.detEvento.tpAutor;
      infEvento.detEvento.verAplic    := RetEventoNF3e.InfEvento.detEvento.verAplic;
      infEvento.detEvento.dhEmi       := RetEventoNF3e.InfEvento.detEvento.dhEmi;
      infEvento.detEvento.tpNF        := RetEventoNF3e.InfEvento.detEvento.tpNF;
      infEvento.detEvento.IE          := RetEventoNF3e.InfEvento.detEvento.IE;

      infEvento.detEvento.dest.UF            := RetEventoNF3e.InfEvento.detEvento.dest.UF;
      infEvento.detEvento.dest.CNPJCPF       := RetEventoNF3e.InfEvento.detEvento.dest.CNPJCPF;
      infEvento.detEvento.dest.idEstrangeiro := RetEventoNF3e.InfEvento.detEvento.dest.idEstrangeiro;
      infEvento.detEvento.dest.IE            := RetEventoNF3e.InfEvento.detEvento.dest.IE;

      infEvento.detEvento.vNF   := RetEventoNF3e.InfEvento.detEvento.vNF;
      infEvento.detEvento.vICMS := RetEventoNF3e.InfEvento.detEvento.vICMS;
      infEvento.detEvento.vST   := RetEventoNF3e.InfEvento.detEvento.vST;

      signature.URI             := RetEventoNF3e.signature.URI;
      signature.DigestValue     := RetEventoNF3e.signature.DigestValue;
      signature.SignatureValue  := RetEventoNF3e.signature.SignatureValue;
      signature.X509Certificate := RetEventoNF3e.signature.X509Certificate;

      if RetEventoNF3e.retEvento.Count > 0 then
      begin
        FRetInfEvento.Id := RetEventoNF3e.retEvento.Items[0].RetInfEvento.Id;
        FRetInfEvento.tpAmb := RetEventoNF3e.retEvento.Items[0].RetInfEvento.tpAmb;
        FRetInfEvento.verAplic := RetEventoNF3e.retEvento.Items[0].RetInfEvento.verAplic;
        FRetInfEvento.cOrgao := RetEventoNF3e.retEvento.Items[0].RetInfEvento.cOrgao;
        FRetInfEvento.cStat := RetEventoNF3e.retEvento.Items[0].RetInfEvento.cStat;
        FRetInfEvento.xMotivo := RetEventoNF3e.retEvento.Items[0].RetInfEvento.xMotivo;
        FRetInfEvento.chNF3e := RetEventoNF3e.retEvento.Items[0].RetInfEvento.chNF3e;
        FRetInfEvento.tpEvento := RetEventoNF3e.retEvento.Items[0].RetInfEvento.tpEvento;
        FRetInfEvento.xEvento := RetEventoNF3e.retEvento.Items[0].RetInfEvento.xEvento;
        FRetInfEvento.nSeqEvento := RetEventoNF3e.retEvento.Items[0].RetInfEvento.nSeqEvento;
        FRetInfEvento.cOrgaoAutor := RetEventoNF3e.retEvento.Items[0].RetInfEvento.cOrgaoAutor;
        FRetInfEvento.CNPJDest := RetEventoNF3e.retEvento.Items[0].RetInfEvento.CNPJDest;
        FRetInfEvento.emailDest := RetEventoNF3e.retEvento.Items[0].RetInfEvento.emailDest;
        FRetInfEvento.dhRegEvento := RetEventoNF3e.retEvento.Items[0].RetInfEvento.dhRegEvento;
        FRetInfEvento.nProt := RetEventoNF3e.retEvento.Items[0].RetInfEvento.nProt;
        FRetInfEvento.XML := RetEventoNF3e.retEvento.Items[0].RetInfEvento.XML;
      end;
    end;
  finally
    RetEventoNF3e.Free;
  end;
end;

function TEventoNF3e.LerFromIni(const AIniString: String; CCe: Boolean): Boolean;
var
  I: Integer;
  sSecao, sFim: String;
  INIRec: TMemIniFile;
  ok: Boolean;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);
    idLote := INIRec.ReadInteger( 'EVENTO', 'idLote', 0);

    I := 1 ;
    while true do
    begin
      sSecao := 'EVENTO'+IntToStrZero(I,3) ;
      sFim   := INIRec.ReadString(  sSecao,'chNF3e'  ,'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.cOrgao   := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ     := INIRec.ReadString( sSecao, 'CNPJ'  , '');
        infEvento.chNF3e   := sFim;
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(    sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoNF3e(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));

        infEvento.nSeqEvento   := INIRec.ReadInteger(sSecao, 'nSeqEvento'  , 1);
        infEvento.versaoEvento := INIRec.ReadString( sSecao, 'versaoEvento', '1.00');;

        infEvento.detEvento.xCorrecao   := INIRec.ReadString( sSecao, 'xCorrecao'  , '');
        infEvento.detEvento.xCondUso    := INIRec.ReadString( sSecao, 'xCondUso'   , '');
        infEvento.detEvento.nProt       := INIRec.ReadString( sSecao, 'nProt'      , '');
        infEvento.detEvento.xJust       := INIRec.ReadString( sSecao, 'xJust'      , '');
        infEvento.detEvento.cOrgaoAutor := INIRec.ReadInteger(sSecao, 'cOrgaoAutor', 0);
        infEvento.detEvento.verAplic    := INIRec.ReadString( sSecao, 'verAplic'   , '1.0');
        infEvento.detEvento.chNF3eRef   := INIRec.ReadString( sSecao, 'chNF3eRef'  , '');
      end;

      Inc(I);
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

end.
