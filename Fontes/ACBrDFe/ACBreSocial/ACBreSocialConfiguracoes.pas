{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}
{$I ACBr.inc}


unit ACBreSocialConfiguracoes;

interface

uses
  Classes, SysUtils, ACBrDFeConfiguracoes, pcnConversao, eSocial_Conversao;

type
  TConfiguracoeseSocial = class(TConfiguracoes)
  private
    function GetArquivos: TArquivosConf;
    function GetGeral: TGeralConf;
  protected
    procedure CreateGeralConf; override;
    procedure CreateArquivosConf; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(DeConfiguracoeseSocial: TConfiguracoeseSocial); overload;
  published
    property Geral: TGeralConf read GetGeral;
    property Arquivos: TArquivosConf read GetArquivos;
    property WebServices;
    property Certificados;
  end;

implementation

uses
   ACBreSocial, ACBrDFeUtil;


{ TConfiguracoeseSocial }

procedure TConfiguracoeseSocial.Assign(DeConfiguracoeseSocial: TConfiguracoeseSocial);
begin
  Geral.Assign(DeConfiguracoeseSocial.Geral);
  WebServices.Assign(DeConfiguracoeseSocial.WebServices);
  Certificados.Assign(DeConfiguracoeseSocial.Certificados);
  Arquivos.Assign(DeConfiguracoeseSocial.Arquivos);
end;

constructor TConfiguracoeseSocial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WebServices.ResourceName := 'ACBreSocialServices';
end;

procedure TConfiguracoeseSocial.CreateArquivosConf;
begin
  FPArquivos := TArquivosConf.Create(self);
end;

procedure TConfiguracoeseSocial.CreateGeralConf;
begin
  FPGeral := TGeralConf.Create(Self);
end;


function TConfiguracoeseSocial.GetArquivos: TArquivosConf;
begin
  Result := TArquivosConf(FPArquivos);
end;


function TConfiguracoeseSocial.GetGeral: TGeralConf;
begin
  Result := TGeralConf(FPGeral);
end;

end.
