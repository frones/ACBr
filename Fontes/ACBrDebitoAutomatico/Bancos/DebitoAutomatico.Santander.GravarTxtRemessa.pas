{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit DebitoAutomatico.Santander.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrDebitoAutomaticoClass, Febraban150.GravarTxtRemessa;

type
 { TArquivoW_Santander }

  TArquivoW_Santander = class(TArquivoW_Febraban150)
  protected
    procedure GerarRegistroA; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrDebitoAutomaticoConversao;

{ TArquivoW_Santander }

procedure TArquivoW_Santander.GerarRegistroA;
begin
  FpLinha := '';

  GravarCampo('A', 1, tcStr);
  GravarCampo(TipoArquivoToStr(DebitoAutomatico.Geral.TipoArquivo), 1, tcStr);
  GravarCampo(BancoToStr(DebitoAutomatico.Geral.Banco), 3, tcStr);
  GravarCampo(DebitoAutomatico.RegistroA.CodigoConvenio, 17, tcStrZero);
  GravarCampo(DebitoAutomatico.RegistroA.NomeEmpresa, 20, tcStr);
  GravarCampo(DebitoAutomatico.RegistroA.CodigoBanco, 3, tcInt);
  GravarCampo(DebitoAutomatico.RegistroA.NomeBanco, 20, tcStr);
  GravarCampo(DebitoAutomatico.RegistroA.Geracao, 8, tcDatISO);
  GravarCampo(DebitoAutomatico.RegistroA.NSA, 6, tcInt);
  GravarCampo(LayoutVersaoToStr(DebitoAutomatico.RegistroA.LayoutVersao), 2, tcStr);
  GravarCampo(IDENTIFICACAOSERVICO, 17, tcStr);
  GravarCampo(' ', 52, tcStr);

  ValidarLinha('A');
  IncluirLinha;
end;

end.
