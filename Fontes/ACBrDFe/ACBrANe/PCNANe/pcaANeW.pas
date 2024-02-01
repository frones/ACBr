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

unit pcaANeW;

interface

uses
  SysUtils, Classes,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  pcnAuxiliar, pcnConversao, pcnGerador,
  ACBrDFeConsts,
  pcaConversao, pcaANe;

type

  TANeW = class(TObject)
  private
    FGerador: TGerador;
    FANe: TANe;
    FTipoDoc: TTipoDoc;
    FSeguradora: TSeguradora;

  public
    constructor Create(AOwner: TANe);
    destructor Destroy; override;
    function GerarXml: Boolean;
    function GerarXml_ATM: Boolean;
    function GerarXml_ELT: Boolean;

    property Gerador: TGerador       read FGerador    write FGerador;
    property ANe: TANe               read FANe        write FANe;
    property TipoDoc: TTipoDoc       read FTipoDoc    write FTipoDoc;
    property Seguradora: TSeguradora read FSeguradora write FSeguradora;
  end;

const
  DSC_USUARIO = 'login: nome do usuário';
  DSC_SENHA = 'login: senha do usuário';
  DSC_CODATM = 'login: codigo AT&M';
  DSC_APLICACAO = 'Nome da Aplicação';
  DSC_ASSUNTO = 'Assunto do e-mail';
  DSC_REMETENTES = 'Remetentes do e-mail';
  DSC_DESTINATARIOS = 'Destinatários do e-mail';
  DSC_CORPO = 'Corpo do e-mail';
  DSC_CHAVE = 'Chave';
  DSC_CHAVERESP = 'Chave Resposta';
  DSC_TAMANHO = 'Tamanho do documento a ser averbado';
  DSC_NOMEARQ = 'Nome do arquivo';

implementation

{ TANeW }

constructor TANeW.Create(AOwner: TANe);
begin
  inherited Create;
  FANe     := AOwner;
  FGerador := TGerador.Create;
end;

destructor TANeW.Destroy;
begin
  FGerador.Free;

  inherited Destroy;
end;

function TANeW.GerarXml: Boolean;
begin
  case Seguradora of
    tsATM: Result := GerarXml_ATM;
    tsELT: Result := GerarXml_ELT;
  else
    Result := False;
  end;
end;

function TANeW.GerarXml_ATM: Boolean;
var
  sTipoDoc: String;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';

  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoTXT := '';

  Gerador.wCampo(tcStr, '#1', 'usuario', 01, 20, 1, ANe.usuario, DSC_USUARIO);
  Gerador.wCampo(tcStr, '#2', 'senha'  , 01, 20, 1, ANe.senha, DSC_SENHA);
  Gerador.wCampo(tcInt, '#3', 'codatm' , 01, 20, 1, ANe.codatm, DSC_CODATM);

  case TipoDoc of
    tdNFe:  sTipoDoc := '<xmlNFe><![CDATA[' + RemoverDeclaracaoXML(ANe.xmlDFe) + ']]></xmlNFe>';
    tdCTe:  sTipoDoc := '<xmlCTe><![CDATA[' + RemoverDeclaracaoXML(ANe.xmlDFe) + ']]></xmlCTe>';
    tdMDFe: sTipoDoc := '<xmlMDFe><![CDATA[' + RemoverDeclaracaoXML(ANe.xmlDFe) + ']]></xmlMDFe>';

    tdAddBackMail: begin
                     Gerador.wCampo(tcStr, '#4', 'aplicacao'    , 01, 20, 1, ANe.aplicacao, DSC_APLICACAO);
                     Gerador.wCampo(tcStr, '#4', 'assunto'      , 01, 20, 1, ANe.assunto, DSC_ASSUNTO);
                     Gerador.wCampo(tcStr, '#4', 'remetentes'   , 01, 20, 1, ANe.remetentes, DSC_REMETENTES);
                     Gerador.wCampo(tcStr, '#4', 'destinatarios', 01, 20, 1, ANe.destinatarios, DSC_DESTINATARIOS);
                     Gerador.wCampo(tcStr, '#4', 'corpo'        , 01, 20, 1, ANe.corpo, DSC_CORPO);
                     Gerador.wCampo(tcStr, '#4', 'chave'        , 01, 20, 1, ANe.chave, DSC_CHAVE);
                     Gerador.wCampo(tcStr, '#4', 'chaveresp'    , 01, 20, 1, ANe.chaveresp, DSC_CHAVERESP);

                     sTipoDoc := '';
                   end;
  else
    sTipoDoc := '';
  end;

  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + sTipoDoc;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TANeW.GerarXml_ELT: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';

  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoTXT := '';

  Gerador.wCampo(tcStr, '#1', 'tem:Length  ', 01, 10, 1, Length(ANe.xmlDFe), DSC_TAMANHO);
  Gerador.wCampo(tcStr, '#2', 'tem:FileName', 01, 44, 1, ANe.NomeArq, DSC_NOMEARQ);
  Gerador.wCampo(tcStr, '#3', 'tem:CNPJ    ', 14, 14, 1, OnlyNumber(ANe.CNPJ), DSC_CNPJ);

  Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

