{******************************************************************************}
{ Projeto: ACBrNFeMonitor                                                      }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}
unit DoPosPrinterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, CmdUnit, ACBrMonitor1;

procedure DoPosPrinter(cmd: TACBrCmd);

implementation

uses
  ACBrPosPrinter, ACBrUtil;

procedure DoPosPrinter(cmd: TACBrCmd);
var
  status: TACBrPosPrinterStatus;
  i: TACBrPosTipoStatus;
  j: TACBrPosPrinterModelo;
  astr: string;
  iModelo: integer;
begin
  with FrmACBrMonitor.ACBrPosPrinter1 do
  begin
    try
      if cmd.Metodo = 'ativar' then
      begin
        FrmACBrMonitor.ConfiguraPosPrinter;
        Ativar;
        FrmACBrMonitor.sbSerial.Enabled := False;
        FrmACBrMonitor.bbAtivar.Caption := 'Desativar';
      end

      else if cmd.Metodo = 'desativar' then
      begin
        Desativar;
        FrmACBrMonitor.bbAtivar.Caption := 'Ativar';
        FrmACBrMonitor.sbSerial.Enabled := True;
      end

      else if cmd.Metodo = 'ativo' then
        cmd.Resposta := BoolToStr(Ativo, True)

      else if cmd.Metodo = 'imprimir' then
        Imprimir(cmd.Params(0))

      else if cmd.Metodo = 'imprimirlinha' then
        ImprimirLinha(cmd.Params(0))

      else if cmd.Metodo = 'imprimircmd' then
        ImprimirCmd(cmd.Params(0))

      else if cmd.Metodo = 'imprimirtags' then
        ImprimirTags

      else if cmd.Metodo = 'lerstatusimpressora' then
      begin
        status := LerStatusImpressora;
        astr := '';

        if status = [] then
          cmd.Resposta := 'Nenhum status encontrado'
        else
        begin
          for i := Low(TACBrPosTipoStatus) to High(TACBrPosTipoStatus) do
          begin
            if i in status then
              astr := astr + GetEnumName(TypeInfo(TACBrPosTipoStatus),
                integer(i)) + ', ';
          end;

          cmd.Resposta := astr;
        end;
      end

      else if cmd.Metodo = 'lerinfoimpressora' then
        cmd.Resposta := LerInfoImpressora

      else if cmd.Metodo = 'modelo' then
        cmd.Resposta := GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(Modelo))
        
      else if cmd.Metodo = 'setmodelo' then
      begin
        iModelo := -1;
        for j := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
        begin
           if Uppercase(GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(j))) = UpperCase(cmd.Params(0)) then
           begin
              iModelo := integer(j);
              break;
           end;
        end;
        if iModelo >= 0 then
        begin
           Modelo := TACBrPosPrinterModelo(iModelo);
           FrmACBrMonitor.cbxModelo.Text := cmd.Params(0);
        end
        else
           raise Exception.Create('Modelo de impressora inválido: '+Cmd.Params(0));
      end

      else if cmd.Metodo = 'porta' then
        cmd.Resposta := Porta

      else if cmd.Metodo = 'setporta' then
      begin
        Porta := cmd.Params(0);
        FrmACBrMonitor.cbxPorta.Text := cmd.Params(0);
      end

      else if cmd.Metodo = 'colunas' then
        cmd.Resposta := IntToStr(Colunas)

      else if cmd.Metodo = 'espacoentrelinhas' then
        cmd.Resposta := IntToStr(EspacoEntreLinhas)

      else if cmd.Metodo = 'setespacoentrelinhas' then
        EspacoEntreLinhas:= StrToInt(cmd.Params(0))

      else if cmd.Metodo = 'linhasentrecupons' then
        cmd.Resposta := IntToStr(LinhasEntreCupons)

      else if cmd.Metodo = 'setlinhasentrecupons' then
       linhasentrecupons:= StrToInt(cmd.Params(0))

      else if cmd.Metodo = 'linhasbuffer' then
        cmd.Resposta := IntToStr(LinhasBuffer)

      else if cmd.Metodo = 'setlinhasbuffer' then
       LinhasBuffer:= StrToInt(cmd.Params(0))

      else if cmd.Metodo = 'colunasfonteexpandida' then
        cmd.Resposta := IntToStr(ColunasFonteExpandida)

      else if cmd.Metodo = 'colunasfontecondensada' then
        cmd.Resposta := IntToStr(ColunasFonteCondensada)

      else if cmd.Metodo = 'paginadecodigo' then
        cmd.Resposta := GetEnumName(TypeInfo(TACBrPosPaginaCodigo),
          integer(PaginaDeCodigo))

      else if cmd.Metodo = 'setpaginadecodigo' then
       PaginaDeCodigo:= TACBrPosPaginaCodigo( StrToInt(cmd.Params(0)) )

      else if cmd.Metodo = 'colunasfontenormal' then
        cmd.Resposta := IntToStr(ColunasFonteNormal)

      else if cmd.Metodo = 'setcolunasfontenormal' then
        ColunasFonteNormal := StrToInt(cmd.Params(0))

      else if cmd.Metodo = 'cortapapel' then
        cmd.Resposta := BoolToStr(CortaPapel, True)

      else if cmd.Metodo = 'setcortapapel' then
      begin
       CortaPapel := StrToBool(cmd.Params(0));
       FrmACBrMonitor.cbCortarPapel.Checked := CortaPapel;
      end

      else
        raise Exception.Create(ACBrStr('Comando invalido (' + Cmd.Comando + ')'));
    finally

    end;
  end;
end;

end.
