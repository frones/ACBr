{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_N;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrECFBlocos;

type
  /// Registro N001 - Abertura do bloco N – Cálculo do IRPJ e da CSLL
  TRegistroN001 = class(TOpenBlocos)
  private
  public
  end;

  /// Registro N030 - Identificação do Período e Forma de Apuração do
  /// IRPJ e da CSLL das Empresas Tributadas pelo
  /// Lucro Real

  { TRegistroN030 }

  TRegistroN030 = class
  private
    fDT_FIN:   TDateTime;
    fDT_INI:   TDateTime;
    fPER_APUR: string;
  public
    property DT_INI: TDateTime read fDT_INI write fDT_INI;
    property DT_FIN: TDateTime read fDT_FIN write fDT_FIN;
    property PER_APUR: string read fPER_APUR write fPER_APUR;
  end;

  /// Registro N500 - Base de Cálculo do IRPJ Sobre o Lucro Real Após
  /// as Compensações de Prejuízos

  { TRegistroN500 }

  TRegistroN500 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N600 - Demonstração do Lucro da Exploração

  { TRegistroN600 }

  TRegistroN600 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N610 - Cálculo da Isenção e Redução do Imposto sobre Lucro Real

  { TRegistroN610 }

  TRegistroN610 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N615 - Informações da Base de Cálculo de Incentivos Fiscais

  { TRegistroN615 }

  TRegistroN615 = class
  private
    fBASE_CALC:    variant;
    fPER_INCEN_FINAM: variant;
    fPER_INCEN_FINOR: variant;
    fPER_INCEN_FUNRES: variant;
    fPER_VL_SUBTOTAL: variant;
    fPER_VL_TOTAL: variant;
    fVL_LIQ_INCEN_FINAM: variant;
    fVL_LIQ_INCEN_FINOR: variant;
    fVL_LIQ_INCEN_FUNRES: variant;
    fVL_SUBTOTAL:  variant;
    fVL_TOTAL:     variant;
  public
    property BASE_CALC: variant read fBASE_CALC write fBASE_CALC;
    property PER_INCEN_FINOR: variant read fPER_INCEN_FINOR write fPER_INCEN_FINOR;
    property VL_LIQ_INCEN_FINOR: variant read fVL_LIQ_INCEN_FINOR write fVL_LIQ_INCEN_FINOR;
    property PER_INCEN_FINAM: variant read fPER_INCEN_FINAM write fPER_INCEN_FINAM;
    property VL_LIQ_INCEN_FINAM: variant read fVL_LIQ_INCEN_FINAM write fVL_LIQ_INCEN_FINAM;
    property VL_SUBTOTAL: variant read fVL_SUBTOTAL write fVL_SUBTOTAL;
    property PER_VL_SUBTOTAL: variant read fPER_VL_SUBTOTAL write fPER_VL_SUBTOTAL;
    property PER_INCEN_FUNRES: variant read fPER_INCEN_FUNRES write fPER_INCEN_FUNRES;
    property VL_LIQ_INCEN_FUNRES: variant read fVL_LIQ_INCEN_FUNRES write fVL_LIQ_INCEN_FUNRES;
    property VL_TOTAL: variant read fVL_TOTAL write fVL_TOTAL;
    property PER_VL_TOTAL: variant read fPER_VL_TOTAL write fPER_VL_TOTAL;
  end;

  /// Registro N620 - Cálculo do IRPJ Mensal por Estimativa

  { TRegistroN620 }

  TRegistroN620 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N630 - Cálculo do IRPJ Com Base no Lucro Real

  { TRegistroN630 }

  TRegistroN630 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N650 - Base de Cálculo da CSLL Após Compensações das
  /// Bases de Cálculo Negativa

  { TRegistroN650 }

  TRegistroN650 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N660 - Cálculo da CSLL Mensal por Estimativa

  { TRegistroN660 }

  TRegistroN660 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N670 -Cálculo da CSLL Com Base no Lucro Real

  { TRegistroN670 }

  TRegistroN670 = class
  private
    fCODIGO:    string;
    fDESCRICAO: string;
    fVALOR:     variant;
  public
    property CODIGO: string read fCODIGO write fCODIGO;
    property DESCRICAO: string read fDESCRICAO write fDESCRICAO;
    property VALOR: variant read fVALOR write fVALOR;
  end;

  /// Registro N990 - ENCERRAMENTO DO BLOCO N
  TRegistroN990 = class(TCloseBlocos)
  end;


implementation

end.
