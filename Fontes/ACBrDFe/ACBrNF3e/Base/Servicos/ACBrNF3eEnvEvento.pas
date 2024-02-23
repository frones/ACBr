{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eEnvEvento;

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
  ACBrNF3eEventoClass, ACBrNF3eConsts;

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
  ACBrNF3eRetEnvEvento,
  ACBrNF3eConversao;

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

  FEvento  := TInfEventoCollection.Create();
end;

destructor TEventoNF3e.Destroy;
begin
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

function TEventoNF3e.GerarXML: string;
var
  i: Integer;
  sDoc, CNPJCPF, xEvento: String;
begin
  for i := 0 to Evento.Count - 1 do
  begin
    Evento.Items[i].InfEvento.id := 'ID'+
                                    Evento.Items[i].InfEvento.TipoEvento +
                                    OnlyNumber(Evento.Items[i].InfEvento.chNF3e) +
                                    Format('%.2d', [Evento.Items[i].InfEvento.nSeqEvento]);

  //  if Length(Evento.Items[i].InfEvento.id) < 54 then
  //    wAlerta('FP04', 'ID', '', 'ID de Evento inválido');

    sDoc := OnlyNumber(Evento.Items[i].InfEvento.CNPJ);

    if EstaVazio(sDoc) then
      sDoc := ExtrairCNPJCPFChaveAcesso(Evento.Items[i].InfEvento.chNF3e);

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

  //    if not ValidarChave(Evento.Items[i].InfEvento.chNF3e) then
  //      Gerador.wAlerta('FP08', 'chNF3e', '', 'Chave de NF3e inválida');

    case Evento.Items[i].InfEvento.tpEvento of
      teCancelamento:
        begin
          xEvento := '<nProt>' + Evento.Items[i].InfEvento.detEvento.nProt + '</nProt>' +
                     '<xJust>' + Evento.Items[i].InfEvento.detEvento.xJust + '</xJust>';
        end;
    else
      xEvento := '';
    end;

    Xml := '<eventoNF3e ' + NAME_SPACE_NF3e + ' versao="' + versao + '">' +
             '<infEvento Id="' + Evento.Items[i].InfEvento.id + '">' +
               '<cOrgao>' + IntToStr(FEvento.Items[i].FInfEvento.cOrgao) + '</cOrgao>' +
               '<tpAmb>' + TipoAmbienteToStr(Evento.Items[i].InfEvento.tpAmb) + '</tpAmb>' +
               CNPJCPF +
               '<chNF3e>' + Evento.Items[i].InfEvento.chNF3e + '</chNF3e>' +
               '<dhEvento>' +
                  FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',
                                 Evento.Items[i].InfEvento.dhEvento) +
                              GetUTC(CodigoUFparaUF(Evento.Items[i].InfEvento.cOrgao),
                                     Evento.Items[i].InfEvento.dhEvento) +
               '</dhEvento>' +
               '<tpEvento>' + Evento.Items[i].InfEvento.TipoEvento + '</tpEvento>' +
               '<nSeqEvento>' + IntToStr(Evento.Items[i].InfEvento.nSeqEvento) + '</nSeqEvento>' +

               '<detEvento versaoEvento="' + Versao + '">' +
                 '<descEvento>' + Evento.Items[i].InfEvento.DescEvento + '</descEvento>' +
                   xEvento +
               '</detEvento>' +

             '</infEvento>' +
           '</eventoNF3e>';


    if Evento.Items[i].signature.URI <> '' then
    begin
      Evento.Items[i].signature.GerarXML;
      Xml := Xml + Evento.Items[i].signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  Result := Xml;
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
    RetEventoNF3e.XmlRetorno := AXML;
    Result := RetEventoNF3e.LerXml;

    with FEvento.New do
    begin
      infEvento.ID            := RetEventoNF3e.RetInfEvento.id;
      infEvento.cOrgao        := RetEventoNF3e.RetInfEvento.cOrgao;
      infEvento.tpAmb         := RetEventoNF3e.RetInfEvento.tpAmb;
//      infEvento.CNPJ          := RetEventoNF3e.RetInfEvento.CNPJ;
      infEvento.chNF3e        := RetEventoNF3e.RetInfEvento.chNF3e;
//      infEvento.dhEvento      := RetEventoNF3e.RetInfEvento.dhEvento;
      infEvento.tpEvento      := RetEventoNF3e.RetInfEvento.tpEvento;
      infEvento.nSeqEvento    := RetEventoNF3e.RetInfEvento.nSeqEvento;

      infEvento.DetEvento.descEvento := RetEventoNF3e.RetInfEvento.xEvento;
      infEvento.DetEvento.nProt      := RetEventoNF3e.RetInfEvento.nProt;
//      infEvento.DetEvento.xJust      := RetEventoNF3e.RetInfEvento.xJust;

      signature.URI             := RetEventoNF3e.signature.URI;
      signature.DigestValue     := RetEventoNF3e.signature.DigestValue;
      signature.SignatureValue  := RetEventoNF3e.signature.SignatureValue;
      signature.X509Certificate := RetEventoNF3e.signature.X509Certificate;

      FRetInfEvento.Id := RetEventoNF3e.RetInfEvento.Id;
      FRetInfEvento.tpAmb := RetEventoNF3e.RetInfEvento.tpAmb;
      FRetInfEvento.verAplic := RetEventoNF3e.RetInfEvento.verAplic;
      FRetInfEvento.cOrgao := RetEventoNF3e.RetInfEvento.cOrgao;
      FRetInfEvento.cStat := RetEventoNF3e.RetInfEvento.cStat;
      FRetInfEvento.xMotivo := RetEventoNF3e.RetInfEvento.xMotivo;
      FRetInfEvento.chNF3e := RetEventoNF3e.RetInfEvento.chNF3e;
      FRetInfEvento.tpEvento := RetEventoNF3e.RetInfEvento.tpEvento;
      FRetInfEvento.xEvento := RetEventoNF3e.RetInfEvento.xEvento;
      FRetInfEvento.nSeqEvento := RetEventoNF3e.RetInfEvento.nSeqEvento;
      FRetInfEvento.cOrgaoAutor := RetEventoNF3e.RetInfEvento.cOrgaoAutor;
      FRetInfEvento.CNPJDest := RetEventoNF3e.RetInfEvento.CNPJDest;
      FRetInfEvento.emailDest := RetEventoNF3e.RetInfEvento.emailDest;
      FRetInfEvento.dhRegEvento := RetEventoNF3e.RetInfEvento.dhRegEvento;
      FRetInfEvento.nProt := RetEventoNF3e.RetInfEvento.nProt;
      FRetInfEvento.XML := RetEventoNF3e.RetInfEvento.XML;
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
    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1 ;
    while true do
    begin
      sSecao := 'EVENTO' + IntToStrZero(I, 3) ;
      sFim   := INIRec.ReadString(sSecao,'chNF3e', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.cOrgao   := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ     := INIRec.ReadString( sSecao, 'CNPJ'  , '');
        infEvento.chNF3e   := sFim;
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(    sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoNF3e(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));

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
