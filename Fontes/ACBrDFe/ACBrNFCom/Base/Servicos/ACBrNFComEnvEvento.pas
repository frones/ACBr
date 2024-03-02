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

unit ACBrNFComEnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
//  ACBrDFeConversao,
  pcnConversao,
  pcnSignature,
  ACBrNFComEventoClass,
  ACBrNFComConsts;

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

    property InfEvento: TInfEvento read FInfEvento write FInfEvento;
    property signature: Tsignature read Fsignature write Fsignature;
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

  { TEventoNFCom }

  TEventoNFCom = class(TObject)
  private
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: string;
    FXml: string;

    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: string;
    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
    function LerFromIni(const AIniString: string; CCe: Boolean = True): Boolean;

    property idLote: Int64 read FidLote write FidLote;
    property Evento: TInfEventoCollection read FEvento write SetEvento;
    property Versao: string read FVersao  write FVersao;
    property Xml: string read FXml write FXml;
  end;

implementation

uses
  IniFiles,
  ACBrDFeUtil, ACBrXmlBase,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.DateTime,
  ACBrNFComRetEnvEvento,
  ACBrNFComConversao;

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

{ TEventoNFCom }

constructor TEventoNFCom.Create;
begin
  inherited Create;

  FEvento  := TInfEventoCollection.Create();
end;

destructor TEventoNFCom.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoNFCom.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
begin
  case tpEvento of
    teCancelamento: Result := IntToStr(Self.idLote) + '-can-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoNFCom.GerarXML: string;
var
  i: Integer;
  sDoc, sModelo, CNPJCPF, xEvento: string;
begin
  sModelo := Copy(OnlyNumber(Evento.Items[i].InfEvento.chNFCom), 21, 2);

  Evento.Items[i].InfEvento.id := 'ID'+
                                  Evento.Items[i].InfEvento.TipoEvento +
                                  OnlyNumber(Evento.Items[i].InfEvento.chNFCom) +
                                  Format('%.3d', [Evento.Items[i].InfEvento.nSeqEvento]);

//  if Length(Evento.Items[i].InfEvento.id) < 54 then
//    wAlerta('FP04', 'ID', '', 'ID de Evento inválido');

  sDoc := OnlyNumber(Evento.Items[i].InfEvento.CNPJ);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento.Items[i].InfEvento.chNFCom);

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

//    if not ValidarChave(Evento.Items[i].InfEvento.chNFCom) then
//      Gerador.wAlerta('FP08', 'chNFCom', '', 'Chave de NFCom inválida');

  case Evento.Items[i].InfEvento.tpEvento of
    teCancelamento:
      begin
        xEvento := '<evCancNFCom>' +
                     '<descEvento>' +
                       Evento.Items[i].InfEvento.DescEvento +
                     '</descEvento>' +
                     '<nProt>' +
                       Evento.Items[i].InfEvento.detEvento.nProt +
                     '</nProt>' +
                     '<xJust>' +
                       Evento.Items[i].InfEvento.detEvento.xJust +
                     '</xJust>' +
                   '</evCancNFCom>';
      end;
  else
    xEvento := '';
  end;

  Xml := '<eventoNFCom ' + NAME_SPACE_NFCom + ' versao="' + versao + '">' +
           '<infEvento Id="' + Evento.Items[i].InfEvento.id + '">' +
             '<cOrgao>' + IntToStr(FEvento.Items[i].FInfEvento.cOrgao) + '</cOrgao>' +
             '<tpAmb>' + TipoAmbienteToStr(Evento.Items[i].InfEvento.tpAmb) + '</tpAmb>' +
             CNPJCPF +
             '<chNFCom>' + Evento.Items[i].InfEvento.chNFCom + '</chNFCom>' +
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
         '</eventoNFCom>';


  if Evento.Items[i].signature.URI <> '' then
  begin
    Evento.Items[i].signature.GerarXML;
    Xml := Xml + Evento.Items[i].signature.Gerador.ArquivoFormatoXML;
  end;

  Result := Xml;
end;

procedure TEventoNFCom.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoNFCom.LerXML(const CaminhoArquivo: string): Boolean;
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

function TEventoNFCom.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoNFCom : TRetEventoNFCom;
begin
  RetEventoNFCom := TRetEventoNFCom.Create;
  try
    RetEventoNFCom.XmlRetorno := AXML;
    Result := RetEventoNFCom.LerXml;

    with FEvento.New do
    begin
      infEvento.ID            := RetEventoNFCom.RetInfEvento.id;
      infEvento.cOrgao        := RetEventoNFCom.RetInfEvento.cOrgao;
      infEvento.tpAmb         := RetEventoNFCom.RetInfEvento.tpAmb;
//      infEvento.CNPJ          := RetEventoNFCom.RetInfEvento.CNPJ;
      infEvento.chNFCom         := RetEventoNFCom.RetInfEvento.chNFCom;
//      infEvento.dhEvento      := RetEventoNFCom.RetInfEvento.dhEvento;
      infEvento.tpEvento      := RetEventoNFCom.RetInfEvento.tpEvento;
      infEvento.nSeqEvento    := RetEventoNFCom.RetInfEvento.nSeqEvento;

//      infEvento.DetEvento.descEvento := RetEventoNFCom.RetInfEvento.DetEvento.descEvento;
//      infEvento.DetEvento.nProt      := RetEventoNFCom.RetInfEvento.DetEvento.nProt;
//      infEvento.DetEvento.xJust      := RetEventoNFCom.RetInfEvento.DetEvento.xJust;

      signature.URI             := RetEventoNFCom.signature.URI;
      signature.DigestValue     := RetEventoNFCom.signature.DigestValue;
      signature.SignatureValue  := RetEventoNFCom.signature.SignatureValue;
      signature.X509Certificate := RetEventoNFCom.signature.X509Certificate;

      FRetInfEvento.Id := RetEventoNFCom.RetInfEvento.Id;
      FRetInfEvento.tpAmb := RetEventoNFCom.RetInfEvento.tpAmb;
      FRetInfEvento.verAplic := RetEventoNFCom.RetInfEvento.verAplic;
      FRetInfEvento.cOrgao := RetEventoNFCom.RetInfEvento.cOrgao;
      FRetInfEvento.cStat := RetEventoNFCom.RetInfEvento.cStat;
      FRetInfEvento.xMotivo := RetEventoNFCom.RetInfEvento.xMotivo;
      FRetInfEvento.chNFCom := RetEventoNFCom.RetInfEvento.chNFCom;
      FRetInfEvento.tpEvento := RetEventoNFCom.RetInfEvento.tpEvento;
      FRetInfEvento.xEvento := RetEventoNFCom.RetInfEvento.xEvento;
      FRetInfEvento.nSeqEvento := RetEventoNFCom.RetInfEvento.nSeqEvento;
      FRetInfEvento.cOrgaoAutor := RetEventoNFCom.RetInfEvento.cOrgaoAutor;
      FRetInfEvento.CNPJDest := RetEventoNFCom.RetInfEvento.CNPJDest;
      FRetInfEvento.emailDest := RetEventoNFCom.RetInfEvento.emailDest;
      FRetInfEvento.dhRegEvento := RetEventoNFCom.RetInfEvento.dhRegEvento;
      FRetInfEvento.nProt := RetEventoNFCom.RetInfEvento.nProt;
      FRetInfEvento.XML := RetEventoNFCom.RetInfEvento.XML;
    end;
  finally
    RetEventoNFCom.Free;
  end;
end;

function TEventoNFCom.LerFromIni(const AIniString: string; CCe: Boolean): Boolean;
var
  I: Integer;
  sSecao, sFim: string;
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
      sFim   := INIRec.ReadString(sSecao,'chNFCom', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.cOrgao   := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ     := INIRec.ReadString( sSecao, 'CNPJ'  , '');
        infEvento.chNFCom   := sFim;
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(    sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoNFCom(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));

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
