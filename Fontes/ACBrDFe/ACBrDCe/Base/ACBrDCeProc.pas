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

unit ACBrDCeProc;

interface

uses
  SysUtils, Classes, ACBrXmlBase,
  pcnConversao, pcnGerador, pcnLeitor, pcnConsts, ACBrDCeConsts;

type

  TProcDCe = class(TObject)
  private
    FGerador: TGerador;
    FPathDCe: String;
    FPathRetConsReciDCe: String;
    FPathRetConsSitDCe: String;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: String;
    FchDCe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;
    FXML_DCe: String;
    FXML_Prot: String;
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: boolean;

    property Gerador: TGerador read FGerador write FGerador;
    property PathDCe: String read FPathDCe write FPathDCe;
    property PathRetConsReciDCe: String read FPathRetConsReciDCe write FPathRetConsReciDCe;
    property PathRetConsSitDCe: String read FPathRetConsSitDCe write FPathRetConsSitDCe;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property verAplic: String read FverAplic write FverAplic;
    property chDCe: String read FchDCe write FchDCe;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property nProt: String read FnProt write FnProt;
    property digVal: String read FdigVal write FdigVal;
    property cStat: Integer read FcStat write FcStat;
    property xMotivo: String read FxMotivo write FxMotivo;
    property Versao: String read FVersao write FVersao;
    property cMsg: Integer read FcMsg write FcMsg;
    property xMsg: String read FxMsg write FxMsg;
    // Usando na Montagem do DCeProc
    property XML_DCe: String read FXML_DCe write FXML_DCe;
    property XML_Prot: String read FXML_Prot write FXML_Prot;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil.Strings, ACBrDCe;

{ TProcDCe }

constructor TProcDCe.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
end;

destructor TProcDCe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TProcDCe.GerarXML: boolean;

function PreencherTAG(const TAG: String; Texto: String): String;
begin
  result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
end;

var
  XMLDCe: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtDCe: String;
  LocLeitor: TLeitor;
  i: Integer;
  ProtLido: Boolean; // Protocolo lido do Arquivo
begin
  (*
  XMLDCe := TStringList.Create;
  XMLinfProt := TStringList.Create;
  XMLinfProt2 := TStringList.Create;
  Gerador.ListaDeAlertas.Clear;

  try
    if (FXML_DCe = '') and (FXML_Prot = '') then
    begin
      ProtLido := False;
      xProtDCe := '';
      FnProt := '';

      // Arquivo DCe
      if not FileExists(FPathDCe)
       then Gerador.wAlerta('XR04', 'DCe', 'DCe', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
       else XMLDCe.LoadFromFile(FPathDCe);

      FchDCe := RetornarConteudoEntre(XMLDCe.Text, 'Id="DCe', '"');
      if trim(FchDCe) = ''
       then Gerador.wAlerta('XR01', 'ID/DCe', 'Numero da chave do DCe', ERR_MSG_VAZIO);

      if (FPathRetConsReciDCe = '') and (FPathRetConsSitDCe = '')
       then begin
        if (FchDCe = '') and (FnProt = '')
         then Gerador.wAlerta('XR06', 'RECIBO/SITUAÇÃO', 'RECIBO/SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
         else ProtLido := True;
       end;

      // Gerar arquivo pelo Recibo do DCe
      if (FPathRetConsReciDCe <> '') and (FPathRetConsSitDCe = '') and (not ProtLido)
       then begin
        if not FileExists(FPathRetConsReciDCe)
         then Gerador.wAlerta('XR06', 'PROTOCOLO', 'PROTOCOLO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
         else begin
          I := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciDCe);
            while LocLeitor.rExtrai(1, 'protDCe', '', i + 1) <> '' do
             begin
               if LocLeitor.rCampo(tcStr, 'chDCe') = FchDCe
                then FnProt := LocLeitor.rCampo(tcStr, 'nProt');
               if trim(FnProt) = ''
                then Gerador.wAlerta('XR01', 'PROTOCOLO/DCe', 'Numero do protocolo', ERR_MSG_VAZIO)
                else begin
                 xProtDCe := LocLeitor.rExtrai(1, 'protDCe', '', i + 1);
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

      // Gerar arquivo pelo arquivo de consulta da situação do DCe
      if (FPathRetConsReciDCe = '') and (FPathRetConsSitDCe <> '') and (not ProtLido)
       then begin
        if not FileExists(FPathRetConsSitDCe)
         then Gerador.wAlerta('XR06', 'SITUAÇÃO', 'SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
         else begin
          XMLinfProt.LoadFromFile(FPathRetConsSitDCe);

          wCstat:=RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');
          if trim(wCstat) = '101'
           then XMLinfProt2.Text:=RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
           else XMLinfProt2.Text:=RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          xProtDCe := '<protDCe versao="' + Versao + '">' +
                        '<infProt>' +
                          PreencherTAG('tpAmb', XMLinfProt.text) +
                          PreencherTAG('verAplic', XMLinfProt.text) +
                          PreencherTAG('chDCe', XMLinfProt.text) +
                          PreencherTAG('dhRecbto', XMLinfProt2.text) +
                          PreencherTAG('nProt', XMLinfProt2.text) +
                          PreencherTAG('digVal', XMLinfProt.text) +
                          PreencherTAG('cStat', XMLinfProt.text) +
                          PreencherTAG('xMotivo', XMLinfProt.text) +
                        '</infProt>' +
                        IIF( (PreencherTAG('cMsg', XMLinfProt.text) <> ''),
                        '<infFisco>' +
                          PreencherTAG('cMsg', XMLinfProt.text) +
                          PreencherTAG('xMsg', XMLinfProt.text) +
                        '</infFisco>',
                        '') +
                       '</protDCe>';
         end;
       end;

      if ProtLido
       then begin
        xProtDCe := '<protDCe versao="' + Versao + '">' +
                      '<infProt>' +
                       '<tpAmb>'+TpAmbToStr(FtpAmb)+'</tpAmb>'+
                       '<verAplic>'+FverAplic+'</verAplic>'+
                       '<chDCe>'+FchDCe+'</chDCe>'+
                       '<dhRecbto>'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',FdhRecbto)+'</dhRecbto>'+
                       '<nProt>'+FnProt+'</nProt>'+
                       '<digVal>'+FdigVal+'</digVal>'+
                       '<cStat>'+IntToStr(FcStat)+'</cStat>'+
                       '<xMotivo>'+FxMotivo+'</xMotivo>'+
                      '</infProt>'+
                      IIF( (cMsg > 0) or (xMsg <> ''),
                      '<infFisco>' +
                        '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                        '<xMsg>' + FxMsg + '</xMsg>' +
                      '</infFisco>',
                      '') +
                     '</protDCe>';
       end;

      FXML_DCe := XMLDCe.Text;
      FXML_Prot := xProtDCe;
    end;

    // Gerar arquivo
    if (Gerador.ListaDeAlertas.Count = 0) and
       (FXML_DCe <> '') and (FXML_Prot <> '') then
    begin
      Gerador.ArquivoFormatoXML := '';
      Gerador.wGrupo(ENCODING_UTF8, '', False);
      Gerador.wGrupo('dceProc versao="' + Versao + '" xmlns="' + ACBRDCE_NAMESPACE, '"');
      Gerador.wTexto('<DCe xmlns' + RetornarConteudoEntre(FXML_DCe, '<DCe xmlns', '</DCe>') + '</DCe>');
      Gerador.wTexto(FXML_Prot);
      Gerador.wGrupo('/dceProc');
    end;

    Result := (Gerador.ListaDeAlertas.Count = 0);

  finally
    XMLDCe.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
  *)
end;

end.

