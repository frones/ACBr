package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cobv;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Spinner;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.SpinnerUtils;

import java.text.SimpleDateFormat;
import java.util.Date;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosConsultarCobrancasCobVFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtDataInicialConsultarCobrancasCobV;
    private EditText txtDataFinalConsultarCobrancasCobV;
    private EditText txtCPFCNPJConsultarCobrancasCobV;
    private Spinner cmbStatusConsultarCobrancasCobV;
    private CheckBox ckbLocationConsultarCobrancasCobV;
    private EditText txtPagAtualConsultarCobrancasCobV;
    private EditText txtItensPorPaginaConsultarCobrancasCobV;
    private Button btnConsultarCobrancasCobV;
    private EditText txtRespostaConsultarCobrancasCobV;
    private Button btnLimparRespostaConsultarCobrancasCobV;

    private String[] status = {"Nenhum", "Ativa", "Concluída", "Removida Pelo Usuário Recebedor", "Removida Pelo PSP"};

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consultar_cobrancas_cobv, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtDataInicialConsultarCobrancasCobV = view.findViewById(R.id.txtDataInicialConsultarCobrancasCobV);
        txtDataFinalConsultarCobrancasCobV = view.findViewById(R.id.txtDataFinalConsultarCobrancasCobV);
        txtCPFCNPJConsultarCobrancasCobV = view.findViewById(R.id.txtCPFCNPJConsultarCobrancasCobV);
        cmbStatusConsultarCobrancasCobV = view.findViewById(R.id.cmbStatusConsultarCobrancasCobV);
        ckbLocationConsultarCobrancasCobV = view.findViewById(R.id.ckbLocationConsultarCobrancasCobV);
        txtPagAtualConsultarCobrancasCobV = view.findViewById(R.id.txtPagAtualConsultarCobrancasCobV);
        txtItensPorPaginaConsultarCobrancasCobV = view.findViewById(R.id.txtItensPorPaginaConsultarCobrancasCobV);
        btnConsultarCobrancasCobV = view.findViewById(R.id.btnConsultarCobrancasCobV);
        txtRespostaConsultarCobrancasCobV = view.findViewById(R.id.txtRespostaConsultarCobrancasCobV);
        btnLimparRespostaConsultarCobrancasCobV = view.findViewById(R.id.btnLimparRespostaConsultarCobrancasCobV);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        SpinnerUtils.preencherSpinner(getContext(), cmbStatusConsultarCobrancasCobV, status);

        btnConsultarCobrancasCobV.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ConsultarCobrancasCobV();
            }
        });

        btnLimparRespostaConsultarCobrancasCobV.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultarCobrancasCobV();
            }
        });

        return view;
    }

    public void ConsultarCobrancasCobV(){
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
        txtRespostaConsultarCobrancasCobV.setText("");
        String result = "";
        String dataInicial = txtDataInicialConsultarCobrancasCobV.getText().toString();
        String dataFinal = txtDataFinalConsultarCobrancasCobV.getText().toString();
        String cpfCnpj = txtCPFCNPJConsultarCobrancasCobV.getText().toString();
        int pagAtual = Integer.parseInt(txtPagAtualConsultarCobrancasCobV.getText().toString());
        int itensPorPagina = Integer.parseInt(txtItensPorPaginaConsultarCobrancasCobV.getText().toString());
        try{
            Date dateInicial = sdf.parse(dataInicial);
            Date dateFinal = sdf.parse(dataFinal);
            double tDateTimeInicial = ACBrPIXCD.convertDateToTDateTime(dateInicial);
            double tDateTimeFinal = ACBrPIXCD.convertDateToTDateTime(dateFinal);
            result = ACBrPIXCD.ConsultarCobrancasCobV(tDateTimeInicial, tDateTimeFinal, cpfCnpj, ckbLocationConsultarCobrancasCobV.isChecked(), cmbStatusConsultarCobrancasCobV.getSelectedItemPosition(), pagAtual, itensPorPagina);
        } catch (Exception ex){
            Log.e("Erro ao Consultar Cobranças CobV", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarCobrancasCobV.setText(result);
        }
    }

    public void LimparRespostaConsultarCobrancasCobV(){
        txtRespostaConsultarCobrancasCobV.setText("");
    }
}