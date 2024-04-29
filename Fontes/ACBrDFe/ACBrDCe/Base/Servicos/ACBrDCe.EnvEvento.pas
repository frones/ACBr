{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDCe.EnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao,
//  ACBrDFeComum.SignatureClass,
  pcnSignature,
  ACBrDCe.EventoClass,
  ACBrDCe.Consts;

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

  { TEventoDCe }

  TEventoDCe = class(TObject)
  private
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: String;
    FXml: String;

    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: string;
    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
    function LerFromIni(const AIniString: String; CCe: Boolean = True): Boolean;

    property idLote: Int64                read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: String               read FVersao  write FVersao;
    property Xml: String                  read FXml     write FXml;
  end;

implementation

uses
  IniFiles,
  ACBrDFeUtil, ACBrXmlBase,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.DateTime,
  ACBrDCeRetEnvEvento,
  ACBrDCeConversao;

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

{ TEventoDCe }

constructor TEventoDCe.Create;
begin
  inherited Create;

  FEvento  := TInfEventoCollection.Create();
end;

destructor TEventoDCe.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoDCe.ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
begin
  case tpEvento of
    teCancelamento: Result := IntToStr(Self.idLote) + '-can-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoDCe.GerarXML: string;
var
  i: Integer;
  sDoc, sModelo, CNPJCPF, xEvento: String;
begin
  sModelo := Copy(OnlyNumber(Evento.Items[i].InfEvento.chDCe), 21, 2);

  Evento.Items[i].InfEvento.id := 'ID'+
                                  Evento.Items[i].InfEvento.TipoEvento +
                                  OnlyNumber(Evento.Items[i].InfEvento.chDCe) +
                                  Format('%.3d', [Evento.Items[i].InfEvento.nSeqEvento]);

//  if Length(Evento.Items[i].InfEvento.id) < 54 then
//    wAlerta('FP04', 'ID', '', 'ID de Evento inválido');

  sDoc := OnlyNumber(Evento.Items[i].InfEvento.CNPJ);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJChaveAcesso(Evento.Items[i].InfEvento.chDCe);

  case Length( sDoc ) of
    14: begin
          CNPJCPF := '<CNPJ>' + sDoc + '</CNPJ>';

//          if not ValidarCNPJ( sDoc ) then
//            Gerador.wAlerta('FP07', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
        end;
    11: begin
          CNPJCPF := '<CPF>' + sDoc + '</CPF>';

//          if not ValidarCPF( sDoc ) then
//            Gerador.wAlerta('FP07', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
        end;
  end;

//    if not ValidarChave(Evento.Items[i].InfEvento.chDCe) then
//      Gerador.wAlerta('FP08', 'chDCe', '', 'Chave de DCe inválida');

  case Evento.Items[i].InfEvento.tpEvento of
    teCancelamento:
      begin
        xEvento := '<evCancDCe>' +
                     '<descEvento>' +
                       Evento.Items[i].InfEvento.DescEvento +
                     '</descEvento>' +
                     '<nProt>' +
                       Evento.Items[i].InfEvento.detEvento.nProt +
                     '</nProt>' +
                     '<xJust>' +
                       Evento.Items[i].InfEvento.detEvento.xJust +
                     '</xJust>' +
                   '</evCancDCe>';
      end;
  else
    xEvento := '';
  end;

  Xml := '<eventoDCe ' + NAME_SPACE_DCe + ' versao="' + versao + '">' +
           '<infEvento Id="' + Evento.Items[i].InfEvento.id + '">' +
             '<cOrgao>' + IntToStr(FEvento.Items[i].FInfEvento.cOrgao) + '</cOrgao>' +
             '<tpAmb>' + TipoAmbienteToStr(Evento.Items[i].InfEvento.tpAmb) + '</tpAmb>' +
             CNPJCPF +
             '<chDCe>' + Evento.Items[i].InfEvento.chDCe + '</chDCe>' +
             '<dhEvento>' +
                FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',
                               Evento.Items[i].InfEvento.dhEvento) +
                            GetUTC(CodigoUFparaUF(Evento.Items[i].InfEvento.cOrgao),
                                   Evento.Items[i].InfEvento.dhEvento) +
             '</dhEvento>' +
             '<tpEvento>' + Evento.Items[i].InfEvento.TipoEvento + '</tpEvento>' +
             '<nSeqEvento>' + IntToStr(Evento.Items[i].InfEvento.nSeqEvento) + '</nSeqEvento>' +

             '<detEvento versaoEvento="' + Versao + '">' +
               xEvento +
             '</detEvento>' +
           '</infEvento>' +
         '</eventoDCe>';


  if Evento.Items[i].signature.URI <> '' then
  begin
    Xml := Xml + Evento.Items[i].signature.GerarXML;
  end;

  Result := Xml;
end;

procedure TEventoDCe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoDCe.LerXML(const CaminhoArquivo: String): Boolean;
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

function TEventoDCe.LerXMLFromString(const AXML: String): Boolean;
var
  RetEventoDCe : TRetEventoDCe;
begin
  RetEventoDCe := TRetEventoDCe.Create;
  try
    RetEventoDCe.XmlRetorno := AXML;
    Result := RetEventoDCe.LerXml;

    with FEvento.New do
    begin
      infEvento.ID            := RetEventoDCe.RetInfEvento.id;
      infEvento.cOrgao        := RetEventoDCe.RetInfEvento.cOrgao;
      infEvento.tpAmb         := RetEventoDCe.RetInfEvento.tpAmb;
//      infEvento.CNPJ          := RetEventoDCe.RetInfEvento.CNPJ;
      infEvento.chDCe         := RetEventoDCe.RetInfEvento.chDCe;
//      infEvento.dhEvento      := RetEventoDCe.RetInfEvento.dhEvento;
      infEvento.tpEvento      := RetEventoDCe.RetInfEvento.tpEvento;
      infEvento.nSeqEvento    := RetEventoDCe.RetInfEvento.nSeqEvento;

//      infEvento.DetEvento.descEvento := RetEventoDCe.RetInfEvento.DetEvento.descEvento;
//      infEvento.DetEvento.nProt      := RetEventoDCe.RetInfEvento.DetEvento.nProt;
//      infEvento.DetEvento.xJust      := RetEventoDCe.RetInfEvento.DetEvento.xJust;

      signature.URI             := RetEventoDCe.signature.URI;
      signature.DigestValue     := RetEventoDCe.signature.DigestValue;
      signature.SignatureValue  := RetEventoDCe.signature.SignatureValue;
      signature.X509Certificate := RetEventoDCe.signature.X509Certificate;

      FRetInfEvento.Id := RetEventoDCe.RetInfEvento.Id;
      FRetInfEvento.tpAmb := RetEventoDCe.RetInfEvento.tpAmb;
      FRetInfEvento.verAplic := RetEventoDCe.RetInfEvento.verAplic;
      FRetInfEvento.cOrgao := RetEventoDCe.RetInfEvento.cOrgao;
      FRetInfEvento.cStat := RetEventoDCe.RetInfEvento.cStat;
      FRetInfEvento.xMotivo := RetEventoDCe.RetInfEvento.xMotivo;
      FRetInfEvento.chDCe := RetEventoDCe.RetInfEvento.chDCe;
      FRetInfEvento.tpEvento := RetEventoDCe.RetInfEvento.tpEvento;
      FRetInfEvento.xEvento := RetEventoDCe.RetInfEvento.xEvento;
      FRetInfEvento.nSeqEvento := RetEventoDCe.RetInfEvento.nSeqEvento;
      FRetInfEvento.cOrgaoAutor := RetEventoDCe.RetInfEvento.cOrgaoAutor;
      FRetInfEvento.CNPJDest := RetEventoDCe.RetInfEvento.CNPJDest;
      FRetInfEvento.emailDest := RetEventoDCe.RetInfEvento.emailDest;
      FRetInfEvento.dhRegEvento := RetEventoDCe.RetInfEvento.dhRegEvento;
      FRetInfEvento.nProt := RetEventoDCe.RetInfEvento.nProt;
      FRetInfEvento.XML := RetEventoDCe.RetInfEvento.XML;
    end;
  finally
    RetEventoDCe.Free;
  end;
end;

function TEventoDCe.LerFromIni(const AIniString: String; CCe: Boolean): Boolean;
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
    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1 ;
    while true do
    begin
      sSecao := 'EVENTO' + IntToStrZero(I, 3) ;
      sFim   := INIRec.ReadString(sSecao,'chDCe', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.cOrgao   := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ     := INIRec.ReadString( sSecao, 'CNPJ'  , '');
        infEvento.chDCe   := sFim;
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrTotpEventoDCe(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));

        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);

        infEvento.detEvento.nProt := INIRec.ReadString( sSecao, 'nProt', '');
        infEvento.detEvento.xJust := INIRec.ReadString( sSecao, 'xJust', '');
      end;

      Inc(I);
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

end.
