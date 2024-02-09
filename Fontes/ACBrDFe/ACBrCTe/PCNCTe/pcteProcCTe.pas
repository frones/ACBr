{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit pcteProcCTe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcnLeitor, pcteConversaoCTe;

type

//  TPcnPadraoNomeProcCTe = (tpnPublico, tpnPrivado);

  TProcCTe = class(TObject)
  private
    FGerador: TGerador;
    FPathCTe: String;
    FPathRetConsReciCTe: String;
    FPathRetConsSitCTe: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchCTe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FId: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;
    FXML_CTe: String;
    FXML_prot: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;
//    function ObterNomeArquivo(const PadraoNome: TPcnPadraoNomeProcCTe = tpnPrivado): String;
    property Gerador: TGerador          read FGerador            write FGerador;
    property PathCTe: String            read FPathCTe            write FPathCTe;
    property PathRetConsReciCTe: String read FPathRetConsReciCTe write FPathRetConsReciCTe;
    property PathRetConsSitCTe: String  read FPathRetConsSitCTe  write FPathRetConsSitCTe;
    property tpAmb: TpcnTipoAmbiente    read FtpAmb              write FtpAmb;
    property verAplic: String           read FverAplic           write FverAplic;
    property chCTe: String              read FchCTe              write FchCTe;
    property dhRecbto: TDateTime        read FdhRecbto           write FdhRecbto;
    property nProt: String              read FnProt              write FnProt;
    property digVal: String             read FdigVal             write FdigVal;
    property cStat: Integer             read FcStat              write FcStat;
    property xMotivo: String            read FxMotivo            write FxMotivo;
    property Id: String                 read FId                 write FId;
    property Versao: String             read FVersao             write FVersao;
    property cMsg: Integer              read FcMsg               write FcMsg;
    property xMsg: String               read FxMsg               write FxMsg;
    // Usando na Montagem do cteProc
    property XML_CTe: String            read FXML_CTe            write FXML_CTe;
    property XML_prot: String           read FXML_prot           write FXML_prot;
  end;

implementation

uses
  StrUtils,
  ACBrDFeConsts,
  ACBrDFeUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  pcteConsts;

{ TProcCTe }

constructor TProcCTe.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
end;

destructor TProcCTe.Destroy;
begin
  FGerador.Free;
  inherited;
end;
(*
function TProcCTe.ObterNomeArquivo(const PadraoNome: TPcnPadraoNomeProcCTe = tpnPrivado): String;
begin
  Result := FchCTe + '-procCTe.xml';
  if PadraoNome = tpnPublico then
  begin
    Result := FnProt + '_v' + Versao + '-procCTe.xml';
  end;
end;
*)
function TProcCTe.GerarXML: boolean;

function PreencherTAG(const TAG: String; Texto: String): String;
begin
  result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
end;

var
  XMLCTe: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtCTe: String;
  xId: String;
  LocLeitor: TLeitor;
  i: Integer;
  ProtLido: Boolean; // Protocolo lido do Arquivo
  Modelo: Integer;
begin
  XMLCTe      := TStringList.Create;
  XMLinfProt  := TStringList.Create;
  XMLinfProt2 := TStringList.Create;
  Gerador.ListaDeAlertas.Clear;

  try
    if (FXML_CTe = '') and (FXML_prot = '') then
    begin
      ProtLido := False;
      xProtCTe := '';
      FnProt := '';

      // Arquivo CTe
      if not FileExists(FPathCTe) then
        Gerador.wAlerta('XR04', 'CTe', 'CTe', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
      else
        XMLCTe.LoadFromFile(FPathCTe);

      FchCTe := RetornarConteudoEntre(XMLCTe.Text, 'Id="CTe', '"');
      if trim(FchCTe) = '' then
        Gerador.wAlerta('XR01', 'ID/CTe', 'Numero da chave do CTe', ERR_MSG_VAZIO);

      if (FPathRetConsReciCTe = '') and (FPathRetConsSitCTe = '') then
      begin
        if (FchCTe = '') and (FnProt = '') then
          Gerador.wAlerta('XR06', 'RECIBO/SITUAÇÃO', 'RECIBO/SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
          ProtLido := True;
      end;

      // Gerar arquivo pelo Recibo do CTe
      if (FPathRetConsReciCTe <> '') and (FPathRetConsSitCTe = '') and
         (not ProtLido) then
      begin
        if not FileExists(FPathRetConsReciCTe) then
          Gerador.wAlerta('XR06', 'PROTOCOLO', 'PROTOCOLO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else begin
          I := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciCTe);
            while LocLeitor.rExtrai(1, 'protCTe', '', i + 1) <> '' do
            begin
              if LocLeitor.rCampo(tcStr, 'chCTe') = FchCTe then
                FnProt := LocLeitor.rCampo(tcStr, 'nProt');
              if trim(FnProt) = '' then
                Gerador.wAlerta('XR01', 'PROTOCOLO/CTe', 'Numero do protocolo', ERR_MSG_VAZIO)
              else begin
                xProtCTe := LocLeitor.rExtrai(1, 'protCTe', '', i + 1); // +'</protCTe>';
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

      // Gerar arquivo pelo arquivo de consulta da situação do CTe
      if (FPathRetConsReciCTe = '') and (FPathRetConsSitCTe <> '') and
         (not ProtLido) then
      begin
        if not FileExists(FPathRetConsSitCTe) then
          Gerador.wAlerta('XR06', 'SITUAÇÃO', 'SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else begin
          XMLinfProt.LoadFromFile(FPathRetConsSitCTe);

          wCstat:=RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');
          if trim(wCstat) = '101' then
          begin //esta cancelada
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>');
            // Na versão 2.00 pode não constar o grupo infCanc no retConsSitCTe
            if XMLinfProt2.Text = '' then
              XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');
          end
          else
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          xId := RetornarConteudoEntre(XMLinfProt2.text, 'Id="', '">');

          xProtCTe :=
                '<protCTe versao="' + Versao + '">' +
                  '<infProt' + IfThen( (xId <> ''), ' Id="' + xId + '">', '>') +
                    PreencherTAG('tpAmb',    XMLinfProt2.text) +
                    PreencherTAG('verAplic', XMLinfProt2.text) +
                    PreencherTAG('chCTe',    XMLinfProt2.text) +
                    PreencherTAG('dhRecbto', XMLinfProt2.text) +
                    PreencherTAG('nProt',    XMLinfProt2.text) +
                    PreencherTAG('digVal',   XMLinfProt2.text) +
                    PreencherTAG('cStat',    XMLinfProt2.text) +
                    PreencherTAG('xMotivo',  XMLinfProt2.text) +
                  '</infProt>' +
                  IfThen( (PreencherTAG('cMsg', XMLinfProt2.text) <> ''),
                  '<infFisco>' +
                    PreencherTAG('cMsg', XMLinfProt2.text) +
                    PreencherTAG('xMsg', XMLinfProt2.text) +
                  '</infFisco>',
                  '') +
                '</protCTe>';
        end;
      end;

      if ProtLido then
      begin
        xProtCTe :=
              '<protCTe versao="' + Versao + '">' +
                '<infProt' + IfThen( (FId <> ''), ' Id="' + FId + '">', '>') +
                  '<tpAmb>' + TpAmbToStr(FtpAmb) + '</tpAmb>' +
                  '<verAplic>' + FverAplic + '</verAplic>' +
                  '<chCTe>' + FchCTe + '</chCTe>' +
                  '<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) + '</dhRecbto>' +
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
              '</protCTe>';
      end;

      FXML_CTe := XMLCTe.Text;
      FXML_prot := xProtCTe;
    end;

    // Gerar arquivo
    if (Gerador.ListaDeAlertas.Count = 0) and
       (FXML_CTe <> '') and (FXML_prot <> '') then
    begin
      FchCTe := RetornarConteudoEntre(FXML_CTe, 'Id="CTe', '"');

      Modelo := StrToIntDef(ExtrairModeloChaveAcesso(FchCTe), 57);

      Gerador.ArquivoFormatoXML := '';
      Gerador.wGrupo(ENCODING_UTF8, '', False);

      case Modelo of
        64: // GTVe (Guia de Transporte de Valores)
          begin
            Gerador.wGrupo('GTVeProc versao="' + Versao + '" ' + NAME_SPACE_CTE, '');
            Gerador.wTexto('<GTVe xmlns' + RetornarConteudoEntre(FXML_CTe, '<GTVe xmlns', '</GTVe>') + '</GTVe>');
            Gerador.wTexto(FXML_prot);
            Gerador.wGrupo('/GTVeProc');
          end;

        67: // CTe OS (Outros Serviços)
          begin
            Gerador.wGrupo('cteOSProc versao="' + Versao + '" ' + NAME_SPACE_CTE, '');
            Gerador.wTexto('<CTeOS xmlns' + RetornarConteudoEntre(FXML_CTe, '<CTeOS xmlns', '</CTeOS>') + '</CTeOS>');
            Gerador.wTexto(FXML_prot);
            Gerador.wGrupo('/cteOSProc');
          end;
      else  // CTe
        begin
          Gerador.wGrupo('cteProc versao="' + Versao + '" ' + NAME_SPACE_CTE, '');
          Gerador.wTexto('<CTe xmlns' + RetornarConteudoEntre(FXML_CTe, '<CTe xmlns', '</CTe>') + '</CTe>');
          Gerador.wTexto(FXML_prot);
          Gerador.wGrupo('/cteProc');
        end;
      end;
    end;

    Result := (Gerador.ListaDeAlertas.Count = 0);

  finally
    XMLCTe.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

end.

