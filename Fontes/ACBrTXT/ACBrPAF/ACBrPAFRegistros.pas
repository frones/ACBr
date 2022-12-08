{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrPAFRegistros;

interface

uses
  SysUtils,
  Classes,
  DateUtils;

type
  TLayoutPAF = (lpPAFECF, lpPAFNFCe);

  /// REGISTRO TIPO X1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF:

  TRegistroX1 = class
  private
    FUF: String;           /// UF do estabelecimento usuário do PAF-ECF
    FCNPJ: String;         /// CNPJ do estabelecimento usuário do PAF-ECF
    FIE: String;           /// Inscrição Estadual do estabelecimento
    FIM: String;           /// Inscrição Municipal do estabelecimento
    FRAZAOSOCIAL: String;  /// Razão Social do estabelecimento
    fInclusaoExclusao: Boolean; /// Validação dos registros colocando ??? nos espaços em branco da razao social
  public
    constructor Create; virtual; /// Create
    property UF: String read FUF write FUF;
    property CNPJ: String read FCNPJ write FCNPJ;
    property IE: String read FIE write FIE;
    property IM: String read FIM write FIM;
    property RAZAOSOCIAL: String read FRAZAOSOCIAL write FRAZAOSOCIAL;

    property InclusaoExclusao: Boolean read fInclusaoExclusao write fInclusaoExclusao default False;
  end;

  // REGISTRO TIPO Z3 - IDENTIFICAÇÃO DO PAF-ECF
  TRegistroX3 = Class
  private
    fLAUDO:  string;         /// Número do Laudo de Análise Funcional
    fNOME:   string;         /// Nome do aplicativo indicado no Laudo de Análise Técnica
    fVERSAO: string;         /// Versão atual do aplicativo indicado no Laudo de Análise Técnica
  public
    property LAUDO: string read fLAUDO write fLAUDO;
    property NOME: string read fNOME write fNOME;
    property VERSAO: string read fVERSAO write fVERSAO;
  end;

  /// REGISTRO TIPO X9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroX9 = class
  private
    FTOT_REG: integer;  /// Total de registros
  public
    property TOT_REG: integer read FTOT_REG write FTOT_REG;
  end;

implementation

{ TRegistroX1 }

constructor TRegistroX1.Create;
begin
  fInclusaoExclusao := False;
end;

end.
