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

unit pcnEnvEventoBPe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnGerador, ACBrDFeConsts,
  pcnBPeConsts,
  pcnEventoBPe, pcnSignature;

type
  EventoException          = class(Exception);

  TInfEventoCollectionItem = class;

  TInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    function Add: TInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TInfEventoCollectionItem = class(TObject)
  private
    FInfEvento: TInfEvento;
    FRetInfEvento: TRetInfEvento;
    Fsignature: Tsignature;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property signature: Tsignature       read Fsignature    write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  { TEventoBPe }

  TEventoBPe = class(TObject)
  private
    FGerador: TGerador;
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: String;

    procedure SetEvento(const Value: TInfEventoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: Boolean;
    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function LerFromIni(const AIniString: String): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): String;

    property Gerador: TGerador            read FGerador write FGerador;
    property idLote: Int64                read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: String               read FVersao  write FVersao;
  end;

implementation

uses
  IniFiles,
  pcnRetEnvEventoBPe, pcnConversaoBPe,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrDFeUtil;

{ TEventoBPe }

constructor TEventoBPe.Create;
begin
  inherited Create;

  FGerador := TGerador.Create;
  FEvento  := TInfEventoCollection.Create;
end;

destructor TEventoBPe.Destroy;
begin
  FGerador.Free;
  FEvento.Free;

  inherited;
end;

function TEventoBPe.GerarXML: Boolean;
var
  i: Integer;
  sDoc: String;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.wGrupo('eventoBPe ' + NAME_SPACE_BPE + ' versao="' + Versao + '"');

  for i := 0 to Evento.Count - 1 do
  begin
    Evento.Items[i].InfEvento.id := 'ID' +
                                      Evento.Items[i].InfEvento.TipoEvento +
                                      OnlyNumber(Evento.Items[i].InfEvento.chBPe) +
                                      Format('%.2d', [Evento.Items[i].InfEvento.nSeqEvento]);

    Gerador.wGrupo('infEvento Id="' + Evento.Items[i].InfEvento.id + '"');

    if Length(Evento.Items[i].InfEvento.id) < 54 then
      Gerador.wAlerta('EP04', 'ID', '', 'ID de Evento inválido');

    Gerador.wCampo(tcInt, 'EP05', 'cOrgao', 01, 02, 1, FEvento.Items[i].FInfEvento.cOrgao);
    Gerador.wCampo(tcStr, 'EP06', 'tpAmb ', 01, 01, 1, TpAmbToStr(Evento.Items[i].InfEvento.tpAmb), DSC_TPAMB);

    sDoc := OnlyNumber( Evento.Items[i].InfEvento.CNPJ );
    case Length( sDoc ) of
     14: begin
           Gerador.wCampo(tcStr, 'EP07', 'CNPJ', 14, 14, 1, sDoc , DSC_CNPJ);
           if not ValidarCNPJ( sDoc ) then Gerador.wAlerta('EP07', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
         end;
     11: begin
           Gerador.wCampo(tcStr, 'EP07', 'CPF', 11, 11, 1, sDoc, DSC_CPF);
           if not ValidarCPF( sDoc ) then Gerador.wAlerta('EP07', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
         end;
    end;

    Gerador.wCampo(tcStr, 'EP08', 'chBPe', 44, 44, 1, Evento.Items[i].InfEvento.chBPe, DSC_CHAVE);

    if not ValidarChave(Evento.Items[i].InfEvento.chBPe) then
      Gerador.wAlerta('EP08', 'chBPe', '', 'Chave de BPe inválida');

    Gerador.wCampo(tcStr, 'EP09', 'dhEvento  ', 01, 50, 1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento.Items[i].InfEvento.dhEvento) +
                                                           GetUTC(CodigoUFparaUF(Evento.Items[i].InfEvento.cOrgao), Evento.Items[i].InfEvento.dhEvento));
    Gerador.wCampo(tcInt, 'EP10', 'tpEvento  ', 06, 06, 1, Evento.Items[i].InfEvento.TipoEvento);
    Gerador.wCampo(tcInt, 'EP11', 'nSeqEvento', 01, 02, 1, Evento.Items[i].InfEvento.nSeqEvento);

    Gerador.wGrupo('detEvento versaoEvento="' +  Versao + '"');

    case Evento.Items[i].InfEvento.tpEvento of
      teCancelamento:
          begin
            Gerador.wGrupo('evCancBPe', 'EP01');
            Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 060, 1, Evento.Items[i].InfEvento.DescEvento);
            Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 015, 1, Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcStr, 'EP04', 'xJust     ', 15, 255, 1, Evento.Items[i].InfEvento.detEvento.xJust);
            Gerador.wGrupo('/evCancBPe');
          end;

      teNaoEmbarque:
          begin
            Gerador.wGrupo('evNaoEmbBPe', 'EP01');
            Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 060, 1, Evento.Items[i].InfEvento.DescEvento);
            Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 015, 1, Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcStr, 'EP04', 'xJust     ', 15, 255, 1, Evento.Items[i].InfEvento.detEvento.xJust);
            Gerador.wGrupo('/evNaoEmbBPe');
          end;

      teAlteracaoPoltrona:
          begin
            Gerador.wGrupo('evAlteraPoltronaBPe', 'EP01');
            Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 60, 1, Evento.Items[i].InfEvento.DescEvento);
            Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 15, 1, Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcInt, 'EP04', 'poltrona  ', 03, 03, 1, Evento.Items[i].InfEvento.detEvento.poltrona);
            Gerador.wGrupo('/evAlteraPoltronaBPe');
          end;

      teExcessoBagagem:
          begin
            Gerador.wGrupo('evExcessoBagagem', 'EP01');
            Gerador.wCampo(tcStr, 'EP02', 'descEvento', 05, 60, 1, Evento.Items[i].InfEvento.DescEvento);
            Gerador.wCampo(tcStr, 'EP03', 'nProt     ', 15, 15, 1, Evento.Items[i].InfEvento.detEvento.nProt);
            Gerador.wCampo(tcInt, 'EP04', 'qBagagem  ', 01, 20, 1, Evento.Items[i].InfEvento.detEvento.qBagagem);
            Gerador.wCampo(tcDe2, 'EP05', 'vTotBag   ', 01, 15, 1, Evento.Items[i].InfEvento.detEvento.vTotBag);
            Gerador.wGrupo('/evExcessoBagagem');
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

  end;
  Gerador.wGrupo('/eventoBPe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TEventoBPe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

function TEventoBPe.LerXML(const CaminhoArquivo: String): Boolean;
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

function TEventoBPe.LerXMLFromString(const AXML: String): Boolean;
var
  RetEventoBPe : TRetEventoBPe;
begin
  RetEventoBPe := TRetEventoBPe.Create;
  try
     RetEventoBPe.Leitor.Arquivo := AXML;
     Result := RetEventoBPe.LerXml;
     with FEvento.New do
      begin
        infEvento.ID           := RetEventoBPe.InfEvento.id;
        infEvento.cOrgao       := RetEventoBPe.InfEvento.cOrgao;
        infEvento.tpAmb        := RetEventoBPe.InfEvento.tpAmb;
        infEvento.CNPJ         := RetEventoBPe.InfEvento.CNPJ;
        infEvento.chBPe        := RetEventoBPe.InfEvento.chBPe;
        infEvento.dhEvento     := RetEventoBPe.InfEvento.dhEvento;
        infEvento.tpEvento     := RetEventoBPe.InfEvento.tpEvento;
        infEvento.nSeqEvento   := RetEventoBPe.InfEvento.nSeqEvento;
        infEvento.VersaoEvento := RetEventoBPe.InfEvento.VersaoEvento;

        infEvento.DetEvento.xCorrecao := RetEventoBPe.InfEvento.DetEvento.xCorrecao;
        infEvento.DetEvento.xCondUso  := RetEventoBPe.InfEvento.DetEvento.xCondUso;
        infEvento.DetEvento.nProt     := RetEventoBPe.InfEvento.DetEvento.nProt;
        infEvento.DetEvento.xJust     := RetEventoBPe.InfEvento.DetEvento.xJust;
        infEvento.DetEvento.poltrona  := RetEventoBPe.InfEvento.DetEvento.poltrona;
        infEvento.DetEvento.qBagagem  := RetEventoBPe.InfEvento.DetEvento.qBagagem;
        infEvento.DetEvento.vTotBag   := RetEventoBPe.InfEvento.DetEvento.vTotBag;

        infEvento.detEvento.cOrgaoAutor := RetEventoBPe.InfEvento.detEvento.cOrgaoAutor;
        infEvento.detEvento.tpAutor     := RetEventoBPe.InfEvento.detEvento.tpAutor;
        infEvento.detEvento.verAplic    := RetEventoBPe.InfEvento.detEvento.verAplic;
        infEvento.detEvento.dhEmi       := RetEventoBPe.InfEvento.detEvento.dhEmi;
        infEvento.detEvento.tpBPe       := RetEventoBPe.InfEvento.detEvento.tpBPe;
        infEvento.detEvento.IE          := RetEventoBPe.InfEvento.detEvento.IE;

        infEvento.detEvento.dest.UF            := RetEventoBPe.InfEvento.detEvento.dest.UF;
        infEvento.detEvento.dest.CNPJCPF       := RetEventoBPe.InfEvento.detEvento.dest.CNPJCPF;
        infEvento.detEvento.dest.idEstrangeiro := RetEventoBPe.InfEvento.detEvento.dest.idEstrangeiro;
        infEvento.detEvento.dest.IE            := RetEventoBPe.InfEvento.detEvento.dest.IE;

        infEvento.detEvento.vNF   := RetEventoBPe.InfEvento.detEvento.vNF;
        infEvento.detEvento.vICMS := RetEventoBPe.InfEvento.detEvento.vICMS;
        infEvento.detEvento.vST   := RetEventoBPe.InfEvento.detEvento.vST;

        signature.URI             := RetEventoBPe.signature.URI;
        signature.DigestValue     := RetEventoBPe.signature.DigestValue;
        signature.SignatureValue  := RetEventoBPe.signature.SignatureValue;
        signature.X509Certificate := RetEventoBPe.signature.X509Certificate;

        if RetEventoBPe.retEvento.Count > 0 then
         begin
           FRetInfEvento.Id := RetEventoBPe.retEvento.Items[0].RetInfEvento.Id;
           FRetInfEvento.tpAmb := RetEventoBPe.retEvento.Items[0].RetInfEvento.tpAmb;
           FRetInfEvento.verAplic := RetEventoBPe.retEvento.Items[0].RetInfEvento.verAplic;
           FRetInfEvento.cOrgao := RetEventoBPe.retEvento.Items[0].RetInfEvento.cOrgao;
           FRetInfEvento.cStat := RetEventoBPe.retEvento.Items[0].RetInfEvento.cStat;
           FRetInfEvento.xMotivo := RetEventoBPe.retEvento.Items[0].RetInfEvento.xMotivo;
           FRetInfEvento.chBPe := RetEventoBPe.retEvento.Items[0].RetInfEvento.chBPe;
           FRetInfEvento.tpEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.tpEvento;
           FRetInfEvento.xEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.xEvento;
           FRetInfEvento.nSeqEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.nSeqEvento;
           FRetInfEvento.CNPJDest := RetEventoBPe.retEvento.Items[0].RetInfEvento.CNPJDest;
           FRetInfEvento.emailDest := RetEventoBPe.retEvento.Items[0].RetInfEvento.emailDest;
           FRetInfEvento.dhRegEvento := RetEventoBPe.retEvento.Items[0].RetInfEvento.dhRegEvento;
           FRetInfEvento.nProt := RetEventoBPe.retEvento.Items[0].RetInfEvento.nProt;
           FRetInfEvento.XML := RetEventoBPe.retEvento.Items[0].RetInfEvento.XML;
         end;
      end;
  finally
     RetEventoBPe.Free;
  end;
end;

function TEventoBPe.LerFromIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, sFim: String;
  ok: Boolean;
  I: Integer;
begin
  Result := True;
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1;
    while true do
    begin
      sSecao := 'EVENTO'+IntToStrZero(I,3);
      sFim   := INIRec.ReadString(sSecao, 'chBPe', 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break;

      with Self.Evento.New do
      begin
        infEvento.chBPe              := INIRec.ReadString(sSecao, 'chBPe', '');
        infEvento.cOrgao             := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ               := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.dhEvento           := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento           := StrToTpEventoBPe(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento         := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.detEvento.xCondUso := '';
        infEvento.detEvento.xJust    := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt    := INIRec.ReadString(sSecao, 'nProt', '');
        infEvento.detEvento.poltrona := INIRec.ReadInteger(sSecao, 'poltrona', 0);
        infEvento.detEvento.qBagagem := INIRec.ReadInteger(sSecao, 'qBagagem', 0);
        infEvento.detEvento.vTotBag  := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotBag', ''), 0);
      end;
      Inc(I);
    end;
  finally
     INIRec.Free;
  end;
end;

function TEventoBPe.ObterNomeArquivo(tpEvento: TpcnTpEvento): String;
begin
  case tpEvento of
     teCCe                       : Result := IntToStr(Self.idLote) + '-cce.xml';     // Carta de Correção Eletrônica
     teCancelamento,
     teCancSubst                 : Result := IntToStr(Self.idLote) + '-can-eve.xml'; // Cancelamento da NFe como Evento
     teManifDestCiencia,
     teManifDestConfirmacao,
     teManifDestDesconhecimento,
     teManifDestOperNaoRealizada : Result := IntToStr(Self.idLote) + '-man-des.xml'; // Manifestação do Destinatário
     teEPEC                      : Result := Evento.Items[0].InfEvento.chBPe + '-ped-epec.xml'; // EPEC
     tePedProrrog1,
     tePedProrrog2               : Result := Evento.Items[0].InfEvento.chBPe + '-ped-prorr.xml';
     teCanPedProrrog1,
     teCanPedProrrog2            : Result := Evento.Items[0].InfEvento.chBPe + '-can-prorr.xml';
   else
     raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

{ TInfEventoCollection }

function TInfEventoCollection.Add: TInfEventoCollectionItem;
begin
  Result := Self.New;
end;

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
  FInfEvento    := TInfEvento.Create;
  Fsignature    := Tsignature.Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  Fsignature.Free;
  FRetInfEvento.Free;
  inherited;
end;

end.
