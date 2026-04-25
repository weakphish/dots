import { writeToProfile } from 'https://deno.land/x/karabinerts/deno.ts'

function main() {
    writeToProfile('Default', [
        layer(['caps_lock', 'return_or_enter'], 'mod-layer').manipulators([
            map('a').to('left_shift'),
            map('s').to('left_control'),
            map('d').to('left_option'),
            map('f').to('left_command'),
            
            map('quote').to('right_shift'),
            map(';').to('right_control'),
            map('l').to('right_option'),
            map('k').to('right_command'),
        ]),

        duoLayer('a', 's').manipulators([
            map('i').to('up_arrow'), 
            map('j').to('left_arrow'), 
            map('k').to('down_arrow'), 
            map('l').to('right_arrow'),
        ])
    ])   
}

main()
