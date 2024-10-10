package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;

import com.acbr.consultacnpj.acbrlibconsultacnpj.demo.databinding.ActivityConfiguracoesConsultaCnpjBinding;
import com.google.android.material.tabs.TabLayoutMediator;

import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;


public class ConfiguracoesConsultaCNPJActivity extends AppCompatActivity {

    private ActivityConfiguracoesConsultaCnpjBinding binding;

    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;
    private ConsultaCNPJApplication application;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityConfiguracoesConsultaCnpjBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        application = (ConsultaCNPJApplication) getApplicationContext();
        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");
    }

    private void configTabLayout() {
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ConfiguracoesConsultaCNPJFragment(), "Configurações");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        TabLayoutMediator mediator = new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        });

        mediator.attach();
    }
}