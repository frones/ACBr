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

unit pcnProcBPe;

interface

uses
  SysUtils, Classes, pcnConversao, ACBrDFeConsts,
  pcnGerador, pcnBPeConsts;

type

  TProcBPe = class(TObject)
  private
    FGerador: TGerador;
    FPathBPe: String;
    FPathRetConsReciBPe: String;
    FPathRetConsSitBPe: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchBPe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;

    // Usando na Montagem do BPeProc
    FXML_BPe: String;
    FXML_prot: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TProcBPe);
    function GerarXML: Boolean;
    property Gerador: TGerador          read FGerador;
    property PathBPe: String            read FPathBPe            write FPathBPe;
    property PathRetConsReciBPe: String read FPathRetConsReciBPe write FPathRetConsReciBPe;
    property PathRetConsSitBPe: String  read FPathRetConsSitBPe  write FPathRetConsSitBPe;
    property tpAmb: TpcnTipoAmbiente    read FtpAmb              write FtpAmb;
    property verAplic: String           read FverAplic           write FverAplic;
    property chBPe: String              read FchBPe              write FchBPe;
    property dhRecbto: TDateTime        read FdhRecbto           write FdhRecbto;
    property nProt: String              read FnProt              write FnProt;
    property digVal: String             read FdigVal             write FdigVal;
    property cStat: Integer             read FcStat              write FcStat;
    property xMotivo: String            read FxMotivo            write FxMotivo;
    property Versao: String             read FVersao             write FVersao;
    property cMsg: Integer              read FcMsg               write FcMsg;
    property xMsg: String               read FxMsg               write FxMsg;

    // Usando na Montagem do BPeProc
    property XML_BPe: String            read FXML_BPe            write FXML_BPe;
    property XML_prot: String           read FXML_prot           write FXML_prot;
  end;

implementation

uses
  pcnLeitor,
  StrUtils,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

{ TProcBPe }

constructor TProcBPe.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
  FnProt   := '';
end;

destructor TProcBPe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TProcBPe.GerarXML: Boolean;

  function PreencherTAG(const TAG: String; Texto: String): String;
  begin
    result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
  end;

var
  XMLBPe: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtBPe: String;
  nProtLoc: String;
  xUF: String;    
  LocLeitor: TLeitor;
  i: Integer;
  ProtLido: Boolean; //Protocolo lido do arquivo
begin
  XMLBPe      := TStringList.Create;
  XMLinfProt  := TStringList.Create;
  XMLinfProt2 := TStringList.Create;
  Gerador.ListaDeAlertas.Clear;

  try
    if (FXML_BPe = '') and (FXML_prot = '') then
    begin
      ProtLido := False;
      xProtBPe := '';

      // Arquivo BPe
      if not FileExists(FPathBPe) then
        Gerador.wAlerta('XR04', 'BPe', 'BPe', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
      else
        XMLBPe.LoadFromFile(FPathBPe);

      FchBPe := RetornarConteudoEntre(XMLBPe.Text, 'Id="BPe', '"');

      if trim(FchBPe) = '' then
        Gerador.wAlerta('XR01', 'ID/BPe', 'Numero da chave do BPe', ERR_MSG_VAZIO);

      if (FPathRetConsReciBPe = '') and (FPathRetConsSitBPe = '') then
      begin
        if (FchBPe = '') and (FnProt = '') then
          Gerador.wAlerta('XR06', 'RECIBO/SITUAÇÃO', 'RECIBO/SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
          ProtLido := True;
      end;

      // Gerar arquivo pelo Recibo da BPe                                       //
      if (FPathRetConsReciBPe <> '') and (FPathRetConsSitBPe = '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsReciBPe) then
          Gerador.wAlerta('XR06', 'PROTOCOLO', 'PROTOCOLO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          I := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciBPe);
            while LocLeitor.rExtrai(1, 'protBPe', '', i + 1) <> '' do
            begin
              if LocLeitor.rCampo(tcStr, 'chBPe') = FchBPe then
                FnProt := LocLeitor.rCampo(tcStr, 'nProt');

              if trim(FnProt) = '' then
                Gerador.wAlerta('XR01', 'PROTOCOLO/BPe', 'Numero do protocolo', ERR_MSG_VAZIO)
              else
              begin
                xProtBPe := LocLeitor.rExtrai(1, 'protBPe', '', i + 1); // +'</protBPe>';
                Gerador.ListaDeAlertas.Clear;
                break;
              end;

              inc(I);
            end;
          finally
            LocLeitor.Free;
          end;
        end;
      end;

      // Gerar arquivo pelo arquivo de consulta da situação do BPe              //
      if (FPathRetConsReciBPe = '') and (FPathRetConsSitBPe <> '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsSitBPe) then
          Gerador.wAlerta('XR06', 'SITUAÇÃO', 'SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          XMLinfProt.LoadFromFile(FPathRetConsSitBPe);

          wCstat := RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');

          if ((trim(wCstat) = '101') or
              (trim(wCstat) = '151') or
              (trim(wCstat) = '155')) then //esta cancelado
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
          else
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          nProtLoc := RetornarConteudoEntre(XMLinfProt2.text, '<nProt>', '</nProt>');

          xProtBPe := '<protBPe versao="' + Versao + '">' +
                       '<infProt Id="ID'+ nProtLoc +'">' +
                        PreencherTAG('tpAmb', XMLinfProt.text) +
                        PreencherTAG('verAplic', XMLinfProt.text) +
                        PreencherTAG('chBPe', XMLinfProt.text) +
                        PreencherTAG('dhRecbto', XMLinfProt2.text) +
                        PreencherTAG('nProt', XMLinfProt2.text) +
                        PreencherTAG('digVal', XMLinfProt.text) +
                        PreencherTAG('cStat', XMLinfProt.text) +
                        PreencherTAG('xMotivo', XMLinfProt.text) +
                       '</infProt>' +
                       IfThen( (PreencherTAG('cMsg', XMLinfProt.text) <> ''),
                       '<infFisco>' +
                        PreencherTAG('cMsg', XMLinfProt.text) +
                        PreencherTAG('xMsg', XMLinfProt.text) +
                       '/<infFisco>',
                       '') +
                      '</protBPe>';
        end;
      end;

      if ProtLido then
      begin
        if Copy(FverAplic, 1, 2) = 'SV' then
          xUF := CodigoUFparaUF(StrToIntDef(Copy(FchBPe, 1, 2), 0))
        else
          xUF := Copy(FverAplic, 1, 2);

        xProtBPe := '<protBPe versao="' + Versao + '">' +
                     '<infProt Id="' + IfThen( Pos('ID', FnProt) > 0, FnProt, 'ID' + FnProt ) + '">' +
                      '<tpAmb>' + TpAmbToStr(FtpAmb) + '</tpAmb>' +
                      '<verAplic>' + FverAplic + '</verAplic>' +
                      '<chBPe>' + FchBPe + '</chBPe>' +
                      '<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) +
                                     GetUTC(xUF, FdhRecbto) + '</dhRecbto>' +
                      '<nProt>' + FnProt + '</nProt>' +
                      '<digVal>' + FdigVal + '</digVal>' +
                      '<cStat>' + IntToStr(FcStat) + '</cStat>' +
                      '<xMotivo>' + FxMotivo + '</xMotivo>' +
                     '</infProt>' +
                     IfThen( (cMsg > 0) or (xMsg <> ''),
                     '<infFisco>' +
                      '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                      '<xMsg>' + FxMsg + '</xMsg>' +
                     '</infFisco>',
                     '') +
                    '</protBPe>';
      end;

      FXML_BPe := XMLBPe.Text;
      FXML_prot := xProtBPe;
    end;

    // Gerar arquivo
    if (Gerador.ListaDeAlertas.Count = 0) and
       (FXML_BPe <> '') and (FXML_prot <> '') then
    begin
      Gerador.ArquivoFormatoXML := '';
      Gerador.wGrupo(ENCODING_UTF8, '', False);
      Gerador.wGrupo('BPeProc versao="' + Versao + '" ' + NAME_SPACE_BPE, '');
      Gerador.wTexto('<BPe xmlns' + RetornarConteudoEntre(FXML_BPe, '<BPe xmlns', '</BPe>') + '</BPe>');
      Gerador.wTexto(FXML_prot);
      Gerador.wGrupo('/BPeProc');
    end;

    Result := (Gerador.ListaDeAlertas.Count = 0);
  finally
    XMLBPe.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

procedure TProcBPe.Assign(Source: TProcBPe);
begin
  PathBPe            := Source.PathBPe;
  PathRetConsReciBPe := Source.PathRetConsReciBPe;
  PathRetConsSitBPe  := Source.PathRetConsSitBPe;
  tpAmb              := Source.tpAmb;
  verAplic           := Source.verAplic;
  chBPe              := Source.chBPe;
  dhRecbto           := Source.dhRecbto;
  nProt              := Source.nProt;
  digVal             := Source.digVal;
  cStat              := Source.cStat;
  xMotivo            := Source.xMotivo;
  Versao             := Source.Versao;
  cMsg               := Source.cMsg;
  xMsg               := Source.xMsg;
  XML_BPe            := Source.XML_BPe;
  XML_prot           := Source.XML_prot;
end;

end.

